use eframe::egui;
use eframe::emath::Numeric;
use eframe::egui::{Color32, Sense, Vec2, Visuals, Ui, Stroke, Pos2, Align2, FontId, FontFamily, Painter, FontDefinitions, FontData};
use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use midir::{Ignore, MidiInput, MidiInputPort, MidiInputPorts};
use musicserver1::{make_ai_table, make_synth_table, replay_slider, start_ai_thread, start_input_thread, start_output_thread, AITable, ChooserTable, SliderValue, SynthTable, Preference, MelodyInfo, Database, start_database_thread, SynthChoice, send_recorded_melody, GuiDatabaseUpdate, Melody, MidiByte, MusicMode, KeySignature, Accidental, VariationControlSliders};
use std::{mem, thread};
use std::cmp::{min, max};
use std::ops::RangeInclusive;
use std::sync::{Arc, Mutex};
use enum_iterator::all;
use bare_metal_modulo::*;
use std::str::FromStr;
use midi_msg::MidiMsg;

fn main() -> anyhow::Result<()> {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Replayer",
        native_options,
        Box::new(|cc| Box::new(ReplayerApp::new(cc).unwrap())),
    );
    Ok(())
}

#[derive(Clone)]
enum MidiScenario {
    StartingUp,
    NoInputPorts(String),
    InputPortSelected { in_port: MidiInputPort },
    MultipleInputPorts { in_ports: MidiInputPorts },
}

#[derive(Clone, Debug)]
pub struct VecTracker<T: Clone> {
    items: Vec<T>,
    tracker: Option<ModNum<usize>>
}

impl <T:Clone> VecTracker<T> {
    pub fn new(items: Vec<T>) -> Self {
        if items.len() > 0 {
            let tracker = Some(ModNum::new(0, items.len()));
            VecTracker {items, tracker}
        } else {
            VecTracker {items, tracker: None}
        }
    }

    pub fn len(&self) -> usize {self.items.len()}

    pub fn is_empty(&self) -> bool {self.items.is_empty()}

    pub fn get(&self) -> Option<&T> {
        self.items.get(self.tracker.unwrap().a())
    }

    pub fn get_mut(&mut self) -> Option<&mut T> {
        self.items.get_mut(self.tracker.unwrap().a())
    }

    pub fn replace_vec(&mut self, new_v: Vec<T>) {
        self.items = new_v;
        self.tracker = if self.items.is_empty() {
            None
        } else {
            let t = self.tracker.map_or(self.items.len() - 1, |t| t.a());
            Some(ModNum::new(t, self.items.len()))
        };
    }

    pub fn at_start(&self) -> bool {
        self.tracker.map_or(false, |t| t == 0)
    }

    pub fn at_end(&self) -> bool {
        self.tracker.map_or(false, |t| t == self.items.len() - 1)
    }

    pub fn go_to_start(&mut self) {
        if !self.is_empty() {
            self.tracker = Some(ModNum::new(0, self.items.len()));
        }
    }

    pub fn go_to_end(&mut self) {
        if !self.is_empty() {
            self.tracker = Some(ModNum::new(self.items.len() - 1, self.items.len()));
        }
    }

    pub fn go_left(&mut self) {
        if !self.is_empty() && !self.at_start()  {
            self.tracker = self.tracker.map(|t| t - 1);
        }
    }

    pub fn go_right(&mut self) {
        if !self.is_empty() && !self.at_end()  {
            self.tracker = self.tracker.map(|t| t + 1);
        }
    }
}

struct ReplayerApp {
    midi_scenario: Arc<Mutex<MidiScenario>>,
    midi_in: Arc<Mutex<Option<MidiInput>>>,
    ai_name: String,
    ai_table: Arc<Mutex<AITable>>,
    human_synth_name: String,
    human_synth_table: Arc<Mutex<SynthTable>>,
    ai_synth_name: String,
    ai_synth_table: Arc<Mutex<SynthTable>>,
    variation_controls: VariationControlSliders,
    replay_delay_slider: Arc<AtomicCell<SliderValue<f64>>>,
    in_port: Option<MidiInputPort>,
    in_port_name: Option<String>,
    variation_pref: Arc<AtomicCell<Preference>>,
    melody_var_info: Arc<Mutex<VecTracker<(MelodyInfo, MelodyInfo)>>>,
    database: Option<Database>,
    dbase2gui: Arc<SegQueue<(MelodyInfo,MelodyInfo)>>,
    gui2dbase: Arc<SegQueue<GuiDatabaseUpdate>>,
    ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Clef {
    Treble, Bass
}

impl Clef {
    pub fn symbol(&self) -> char {
        match self {
            Self::Treble => '\u{1d11e}',
            Self::Bass => '\u{1d122}'
        }
    }

    pub fn key_signature_positions(&self, sig: &KeySignature) -> Vec<MidiByte> {
        match self {
            Self::Treble => sig.treble_clef(),
            Self::Bass => sig.bass_clef()
        }
    }

    fn size(&self) -> f32 {
        match self {
            Self::Treble => 13.5,
            Self::Bass => 8.0
        }
    }

    fn x_offset(&self) -> f32 {
        10.0
    }

    fn y_offset(&self) -> f32 {
        match self {
            Self::Treble => 5.0,
            Self::Bass => -0.45
        }
    }

    fn render(&self, painter: &Painter, x: f32, y: f32, y_per_pitch: f32) {
        painter.text(Pos2 {x: x + self.x_offset(), y: y + self.y_offset() * y_per_pitch}, Align2::CENTER_CENTER, self.symbol(), ReplayerApp::font_id(self.size() * y_per_pitch), Color32::BLACK);
    }
}

const MIDDLE_C: MidiByte = 60;
const STAFF_PITCH_WIDTH: MidiByte = 19;
const LOWEST_STAFF_PITCH: MidiByte = MIDDLE_C - STAFF_PITCH_WIDTH;
const HIGHEST_STAFF_PITCH: MidiByte = MIDDLE_C + STAFF_PITCH_WIDTH;
const BORDER_SIZE: f32 = 8.0;
const Y_OFFSET: f32 = BORDER_SIZE * 2.0;
const X_OFFSET: f32 = BORDER_SIZE * 5.0;
const ACCIDENTAL_SIZE_MULTIPLIER: f32 = 5.0;
const KEY_SIGNATURE_OFFSET: f32 = 28.0;

macro_rules! load_font {
    ($fonts:ident, $filename:literal) => {
        {
            let name = $filename.split("/").last().unwrap().split(".").next().unwrap().to_owned();
            println!("Loading font {name} from {}.", $filename);
            $fonts.font_data.insert(name.clone(),
                                   FontData::from_static(include_bytes!($filename)));
            $fonts.families.get_mut(&FontFamily::Proportional).unwrap().push(name);
        }
    }
}

impl ReplayerApp {
    fn new(cc: &eframe::CreationContext<'_>) -> anyhow::Result<Self> {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_visuals.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.
        let mut fonts = FontDefinitions::default();
        load_font!(fonts, "../../bravura/BravuraText.otf");
        cc.egui_ctx.set_fonts(fonts);

        let ai_table = make_ai_table();
        let human_synth_table = make_synth_table();
        let ai_synth_table = make_synth_table();
        let ai_name = ai_table.current_name().to_string();
        let human_synth_name = human_synth_table.current_name().to_string();
        let ai_synth_name = ai_synth_table.current_name().to_string();
        let ai_table = Arc::new(Mutex::new(ai_table));
        let human_synth_table = Arc::new(Mutex::new(human_synth_table));
        let ai_synth_table = Arc::new(Mutex::new(ai_synth_table));
        let variation_controls = VariationControlSliders::new();
        let replay_delay_slider = Arc::new(AtomicCell::new(replay_slider()));
        let mut database = Database::new();
        let (melody_var_info, variation_pref) = {
            let mut melody_var_info = VecTracker::new(database.get_melody_pairs().unwrap());
            melody_var_info.go_to_end();
            let variation_pref = melody_var_info.get()
                .map_or(Preference::Neutral,|(_, v)| v.rating());
            (Arc::new(Mutex::new(melody_var_info)),
             Arc::new(AtomicCell::new(variation_pref)))
        };

        let app = ReplayerApp {
            midi_scenario: Arc::new(Mutex::new(MidiScenario::StartingUp)),
            midi_in: Arc::new(Mutex::new(None)),
            variation_controls,
            replay_delay_slider,
            ai_table,
            human_synth_table,
            ai_synth_name,
            ai_synth_table,
            ai_name,
            human_synth_name,
            in_port: None,
            in_port_name: None,
            variation_pref,
            melody_var_info,
            database: Some(database),
            dbase2gui: Arc::new(SegQueue::new()),
            gui2dbase: Arc::new(SegQueue::new()),
            ai2output: Arc::new(SegQueue::new())
        };
        app.startup();
        Ok(app)
    }

    fn main_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading(format!("Replayer ({})", self.in_port_name.as_ref().unwrap()));
            ui.horizontal(|ui| {
                Self::radio_choice(ui,"Human Synthesizer",self.human_synth_table.clone(),&mut self.human_synth_name);
                Self::radio_choice(ui,"Variation Synthesizer",self.ai_synth_table.clone(),&mut self.ai_synth_name);
                Self::radio_choice(ui,"Variation Algorithm",self.ai_table.clone(),&mut self.ai_name);
            });
            Self::update_table_choice(self.ai_table.clone(), self.ai_name.as_str());
            Self::update_table_choice(self.human_synth_table.clone(),self.human_synth_name.as_str());
            Self::update_table_choice(self.ai_synth_table.clone(), self.ai_synth_name.as_str());

            Self::insert_slider(ui, self.variation_controls.whimsification_slider.clone(), "Portion of Suffix to Whimsify");
            Self::insert_slider(ui,self.variation_controls.p_random_slider.clone(),"Probability of Randomization");
            Self::insert_slider(ui,self.variation_controls.p_ornament_slider.clone(),"Probability of Inserting Ornament");
            Self::insert_slider(ui, self.replay_delay_slider.clone(), "Replay Delay (seconds)");
            Self::insert_slider(ui, self.variation_controls.shortest_note_slider.clone(), "Shortest Playable Note (seconds)");
            let empty = {
                let melody_var_info = self.melody_var_info.lock().unwrap();
                melody_var_info.is_empty()
            };

            if !empty {
                self.display_melody_info(ui);
            }
        });
    }

    fn display_melody_info(&mut self, ui: &mut Ui) {
        let mut melody_var_info = self.melody_var_info.lock().unwrap();
        let (melody_info, variation_info) = melody_var_info.get().cloned().unwrap();

        ui.horizontal(|ui| {
            if !melody_var_info.at_start() && ui.button("<").clicked() {
                melody_var_info.go_left();
                let (_, variation_info) = melody_var_info.get().unwrap();
                self.variation_pref.store(variation_info.rating());
            }
            let start_variation_pref = self.variation_pref.load();
            Self::preference_buttons(ui, self.variation_pref.clone());
            if !melody_var_info.at_end() && ui.button(">").clicked() {
                melody_var_info.go_right();
                let (_, variation_info) = melody_var_info.get().unwrap();
                self.variation_pref.store(variation_info.rating());
            }
            if start_variation_pref != self.variation_pref.load() {
                melody_var_info.get_mut().unwrap().0.set_rating(self.variation_pref.load());
                melody_var_info.get_mut().unwrap().1.set_rating(self.variation_pref.load());
                self.gui2dbase.push(melody_var_info.get().cloned().unwrap().1.get_update());
            }
        });

        ui.vertical(|ui| {
            Self::melody_buttons(self.ai2output.clone(), ui, &melody_info, SynthChoice::Human);
            Self::melody_buttons(self.ai2output.clone(), ui, &variation_info,  SynthChoice::Ai);
        });

        let size = Vec2::new(ui.available_width(), (ui.min_size().y - ui.available_height()) / 2.0);
        MelodyRenderer::render(ui, size, &vec![(melody_info.melody(), Color32::BLACK), (variation_info.melody(), Color32::RED)]);
    }

    fn font_id(size: f32) -> FontId {
        FontId {size, family: FontFamily::Proportional}
    }

    fn melody_buttons(ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>, ui: &mut Ui, info: &MelodyInfo, synth: SynthChoice) {
        ui.horizontal(|ui| {
            ui.label(info.date_time_stamp());
            ui.label(info.get_scale_name());
            if ui.button("Play").clicked() {
                Self::play_melody_thread(ai2output,info.melody().clone(), synth);
            }
        });
    }

    fn play_melody_thread(ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>, melody: Melody, synth: SynthChoice) {
        thread::spawn(move || {
            send_recorded_melody(&melody, synth, ai2output);
        });
    }

    fn preference_buttons(ui: &mut Ui, pref: Arc<AtomicCell<Preference>>) {
        let mut current_value = pref.load();
        ui.horizontal(|ui| {
            for preference in all::<Preference>() {
                ui.radio_value(&mut current_value, preference, preference.to_string());
            }
        });
        pref.store(current_value);
    }

    fn radio_choice<T: Clone>(ui: &mut Ui, header: &str, table: Arc<Mutex<ChooserTable<T>>>,
                              tag: &mut String) {
        ui.vertical(|ui| {
            let table = table.lock().unwrap();
            ui.label(header);
            for item in table.name_vec() {
                ui.radio_value(tag, item.clone(), item.clone());
            }
        });
    }

    fn update_table_choice<T: Clone>(table: Arc<Mutex<ChooserTable<T>>>, tag: &str) {
        let mut table = table.lock().unwrap();
        table.choose(tag);
    }

    fn insert_slider<N:FromStr + Numeric>(ui: &mut Ui, slider: Arc<AtomicCell<SliderValue<N>>>, text: &str) {
        let sv = slider.load();
        let mut value = sv.current();
        let range = sv.make_range();
        ui.add(egui::Slider::new(&mut value, range).text(text));
        slider.store(sv.slid_to(value));
    }

    fn startup_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Replayer: Looking for MIDI devices...\nMove mouse to continue");
        });
    }

    fn startup(&self) {
        let mut midi_in = MidiInput::new("midir reading input");
        let scenario = match midi_in {
            Ok(ref mut midi_in) => {
                midi_in.ignore(Ignore::None);
                let in_ports = midi_in.ports();
                match in_ports.len() {
                    0 => MidiScenario::NoInputPorts("No MIDI devices found".to_string()),
                    1 => MidiScenario::InputPortSelected {
                        in_port: in_ports[0].clone(),
                    },
                    _ => MidiScenario::MultipleInputPorts {
                        in_ports: in_ports.clone(),
                    },
                }
            }
            Err(e) => MidiScenario::NoInputPorts(e.to_string()),
        };
        {
            let mut midi_scenario = self.midi_scenario.lock().unwrap();
            *midi_scenario = scenario;
        }
        let mut midi_in_record = self.midi_in.lock().unwrap();
        *midi_in_record = midi_in.ok();
    }

    fn no_midi_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame, message: &str) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Replayer: No MIDI Devices Available");
            ui.label(message);
        });
    }

    fn pick_midi_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame, in_ports: &MidiInputPorts) {
        if self.in_port.is_none() {
            self.in_port = Some(in_ports[0].clone());
        }
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Replayer: Choose a MIDI Device");
            ui.vertical(|ui| {
                for in_port in in_ports.iter() {
                    self.set_in_port_name(in_port);
                    ui.radio_value(
                        &mut self.in_port,
                        Some(in_port.clone()),
                        self.in_port_name.as_ref().unwrap().clone(),
                    );
                }
            });
            if ui.button("Start Playing").clicked() {
                {
                    let mut midi_scenario = self.midi_scenario.lock().unwrap();
                    *midi_scenario = MidiScenario::InputPortSelected {
                        in_port: self.in_port.clone().unwrap(),
                    };
                }
                self.start_now(ctx);
            }
        });
    }

    fn start_now(&mut self, ctx: &egui::Context) {
        let in_port = self.in_port.as_ref().unwrap().clone();
        self.set_in_port_name(&in_port);
        let mut midi_in = None;
        {
            let mut self_midi_in = self.midi_in.lock().unwrap();
            mem::swap(&mut midi_in, &mut *self_midi_in);
        }
        let input2ai = Arc::new(SegQueue::new());
        let ai2dbase = Arc::new(SegQueue::new());
        let mut database = None;
        mem::swap(&mut database, &mut self.database);

        self.start_update_from_database_thread(ctx);

        start_output_thread(
            self.ai2output.clone(),
            self.human_synth_table.clone(),
            self.ai_synth_table.clone(),
        );
        start_ai_thread(
            self.ai_table.clone(),
            input2ai.clone(),
            self.ai2output.clone(),
            ai2dbase.clone(),
            self.variation_controls.clone(),
            self.replay_delay_slider.clone(),
        );
        start_input_thread(
            input2ai,
            midi_in.unwrap(),
            self.in_port.as_ref().unwrap().clone(),
        );
        start_database_thread(self.dbase2gui.clone(), self.gui2dbase.clone(), ai2dbase, database.unwrap());
    }

    fn start_update_from_database_thread(&self, ctx: &egui::Context) {
        let ctx = ctx.clone();
        let dbase2gui = self.dbase2gui.clone();
        let variation_pref = self.variation_pref.clone();
        let melody_var_info = self.melody_var_info.clone();
        thread::spawn(move || {
            loop {
                if let Some((player_info, variation_info)) = dbase2gui.pop() {
                    variation_pref.store(variation_info.rating());
                    let mut melody_var_info = melody_var_info.lock().unwrap();
                    melody_var_info.items.push((player_info, variation_info));
                    melody_var_info.go_to_end();
                    ctx.request_repaint();
                }
            }
        });
    }

    fn set_in_port_name(&mut self, in_port: &MidiInputPort) {
        let midi_in = self.midi_in.lock().unwrap();
        self.in_port_name = midi_in.as_ref().and_then(|m| m.port_name(in_port).ok());
    }
}

/// Musical symbols are a very tricky issue. Here are resources I've used:
/// * Font: [Bravura](https://github.com/steinbergmedia/bravura)
/// * [Unicode for a few symbols](https://www.compart.com/en/unicode/block/U+2600)
/// * [Unicode for the remaining symbols](https://unicode.org/charts/PDF/U1D100.pdf)
struct MelodyRenderer {
    scale: MusicMode,
    sig: KeySignature,
    x_range: RangeInclusive<f32>,
    y_per_pitch: f32,
    y_middle_c: f32,
    hi: MidiByte
}

impl MelodyRenderer {
    fn staff_line_space(&self) -> f32 {
        self.y_per_pitch * 2.0
    }

    fn space_above_staff(&self) -> f32 {
        1.0 + self.scale.diatonic_steps_between_round_up(HIGHEST_STAFF_PITCH, self.hi) as f32
    }

    fn min_x(&self) -> f32 {
        *self.x_range.start()
    }

    fn total_x(&self) -> f32 {
        *self.x_range.end() - self.min_x()
    }

    fn note_offset_x(&self) -> f32 {
        self.min_x() + X_OFFSET + KEY_SIGNATURE_OFFSET + self.y_per_pitch * self.sig.len() as f32
    }

    fn render(ui: &mut Ui, size: Vec2, melodies: &Vec<(&Melody,Color32)>) {
        if melodies.len() > 0 {
            let (response, painter) = ui.allocate_painter(size, Sense::hover());
            let scale = melodies[0].0.best_scale_for();
            let (lo, hi) = Self::min_max_staff(&scale, melodies);
            let num_diatonic_pitches = 1 + scale.diatonic_steps_between(lo, hi).pure_degree().unwrap();
            let y_per_pitch = ((response.rect.max.y - response.rect.min.y) - BORDER_SIZE * 2.0) / num_diatonic_pitches as f32;
            let y_border = Y_OFFSET + response.rect.min.y;
            let renderer = MelodyRenderer {
                hi,
                scale,
                y_per_pitch,
                x_range: response.rect.min.x + BORDER_SIZE..=response.rect.max.x - BORDER_SIZE,
                sig: scale.key_signature(),
                y_middle_c: y_border + y_per_pitch * scale.diatonic_steps_between_round_up(MIDDLE_C, hi) as f32,
            };
            renderer.draw_staff(&painter, Clef::Treble, y_border + y_per_pitch * renderer.space_above_staff());
            renderer.draw_staff(&painter, Clef::Bass, renderer.y_middle_c + renderer.staff_line_space());
            for (melody, color) in melodies.iter().rev() {
                renderer.draw_melody(&painter, melody, *color);
            }
        }
    }

    fn draw_melody(&self, painter: &Painter, melody: &Melody, color: Color32) {
        let mut total_duration = 0.0;
        for note in melody.iter() {
            let x = self.note_offset_x() + self.total_x() * total_duration / melody.duration() as f32;
            total_duration += note.duration() as f32;
            if !note.is_rest() {
                let (staff_offset, auxiliary_symbol) = self.scale.staff_position(note.pitch());
                let y = self.y_middle_c - staff_offset as f32 * self.y_per_pitch;
                painter.circle_filled(Pos2 {x, y}, self.y_per_pitch, color);
                if let Some(auxiliary_symbol) = auxiliary_symbol {
                    self.draw_accidental(&painter, auxiliary_symbol, x + self.staff_line_space(), y);
                }
            }
        }
    }

    fn draw_staff(&self, painter: &Painter, clef: Clef, start_y: f32) {
        let mut y = start_y;
        clef.render(painter, self.min_x(), y, self.y_per_pitch);
        for _ in 0..5 {
            painter.hline(self.x_range.clone(), y, Stroke { width: 1.0, color: Color32::BLACK });
            y += self.staff_line_space();
        }
        for (i, position) in clef.key_signature_positions(&self.sig).iter().enumerate() {
            self.draw_accidental(painter, self.sig.symbol(), self.min_x() + KEY_SIGNATURE_OFFSET + self.y_per_pitch * i as f32, self.y_middle_c - *position as f32 * self.y_per_pitch);
        }
    }

    fn draw_accidental(&self, painter: &Painter, text: Accidental, x: f32, y: f32) {
        painter.text(Pos2 {x, y}, Align2::CENTER_CENTER, text.symbol(), ReplayerApp::font_id(ACCIDENTAL_SIZE_MULTIPLIER * self.y_per_pitch), Color32::BLACK);
    }

    fn min_max_staff(scale: &MusicMode, melodies: &Vec<(&Melody,Color32)>) -> (MidiByte, MidiByte) {
        let mut lo = LOWEST_STAFF_PITCH;
        let mut hi = HIGHEST_STAFF_PITCH;
        for (melody,_) in melodies.iter() {
            let (mlo, mhi) = melody.min_max_pitches();
            lo = min(lo, mlo);
            hi = max(hi, mhi);
        }
        (scale.closest_pitch_below(lo), scale.closest_pitch_above(hi))
    }
}

impl eframe::App for ReplayerApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        ctx.set_visuals(Visuals::light());
        let scenario = {
            let midi_scenario = self.midi_scenario.lock().unwrap();
            midi_scenario.clone()
        };
        match scenario {
            MidiScenario::StartingUp => {
                self.startup_screen(ctx, frame);
            }
            MidiScenario::NoInputPorts(msg) => {
                self.no_midi_screen(ctx, frame, msg.as_str());
            }
            MidiScenario::InputPortSelected { in_port } => {
                if self.in_port.is_none() {
                    self.in_port = Some(in_port);
                    self.start_now(ctx);
                }
                self.main_screen(ctx, frame)
            }
            MidiScenario::MultipleInputPorts { in_ports } => {
                self.pick_midi_screen(ctx, frame, &in_ports);
            }
        }
    }
}