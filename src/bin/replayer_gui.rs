use bare_metal_modulo::*;
use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use eframe::egui::{self, Key, TextEdit};
use eframe::egui::{
    Align2, Color32, FontData, FontDefinitions, FontFamily, FontId, Painter, Pos2, Sense, Stroke,
    Ui, Vec2, Visuals,
};
use eframe::emath::Numeric;
use eframe::epaint::Rect;
use enum_iterator::all;
use midi_fundsp::io::{start_input_thread, start_output_thread, Speaker, SynthMsg};
use midi_fundsp::SynthFunc;
use midir::{Ignore, InitError, MidiInput, MidiInputPort, MidiInputPorts};
use musicserver1::ai_variation::{
    make_ai_table, start_ai_thread, AIFuncType, DEFAULT_AI_NAME, NO_AI_NAME,
};
use musicserver1::analyzer::{Accidental, KeySignature, Melody, MidiByte, MusicMode, Note, MelodicFigure};
use musicserver1::database::{
    start_database_thread, Database, DatabaseGuiUpdate, FromAiMsg, GuiDatabaseUpdate, MelodyInfo,
    Preference, VariationStats,
};
use musicserver1::runtime::{
    make_synth_table, replay_slider, send_recorded_melody, send_two_melodies, ChooserTable,
    MelodyRunStatus, SliderValue, SynthChoice, VariationControls, HUMAN_SPEAKER, VARIATION_SPEAKER,
};
use std::cmp::{max, min};
use std::collections::{VecDeque, HashMap};
use std::fmt::Display;
use std::ops::RangeInclusive;
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

const NUM_OUTPUT_CHANNELS: usize = 2; // More than this, and it has occasional noise-clipping problems.

fn main() {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Replayer",
        native_options,
        Box::new(|cc| Box::new(ReplayerApp::new(cc).unwrap())),
    )
    .unwrap();
}

#[derive(Clone)]
enum MidiScenario {
    StartingUp,
    NoInputPorts(String),
    InputPortSelected { in_port: MidiInputPort },
    MultipleInputPorts { in_ports: MidiInputPorts },
}

impl MidiScenario {
    fn new(midi_in: &mut Result<MidiInput, InitError>) -> Self {
        match midi_in {
            Ok(ref mut midi_in) => {
                midi_in.ignore(Ignore::None);
                let in_ports = midi_in.ports();
                match in_ports.len() {
                    0 => MidiScenario::NoInputPorts(
                        "No MIDI devices found\nRestart program after MIDI device plugged in"
                            .to_string(),
                    ),
                    1 => MidiScenario::InputPortSelected {
                        in_port: in_ports[0].clone(),
                    },
                    _ => MidiScenario::MultipleInputPorts {
                        in_ports: in_ports.clone(),
                    },
                }
            }
            Err(e) => MidiScenario::NoInputPorts(e.to_string()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VecTracker<T: Clone> {
    items: Vec<T>,
    tracker: Option<ModNum<usize>>,
}

impl<T: Clone> VecTracker<T> {
    pub fn new(items: Vec<T>) -> Self {
        if items.len() > 0 {
            let tracker = Some(ModNum::new(0, items.len()));
            VecTracker { items, tracker }
        } else {
            VecTracker {
                items,
                tracker: None,
            }
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn get(&self) -> Option<&T> {
        self.tracker.and_then(|t| self.items.get(t.a()))
    }

    pub fn get_mut(&mut self) -> Option<&mut T> {
        self.items.get_mut(self.tracker.unwrap().a())
    }

    pub fn replace_vec(&mut self, new_v: Vec<T>) {
        self.items = new_v;
        self.tracker = if self.items.is_empty() {
            None
        } else {
            Some(ModNum::new(self.items.len() - 1, self.items.len()))
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
        if !self.is_empty() && !self.at_start() {
            self.tracker = self.tracker.map(|t| t - 1);
        }
    }

    pub fn go_right(&mut self) {
        if !self.is_empty() && !self.at_end() {
            self.tracker = self.tracker.map(|t| t + 1);
        }
    }

    pub fn add(&mut self, item: T) {
        self.items.push(item);
        self.go_to_end();
    }
}

struct TableInfo<T: Clone> {
    name: String,
    table: Arc<Mutex<ChooserTable<T>>>,
    index: Arc<AtomicCell<usize>>,
}

impl<T: Clone> TableInfo<T> {
    fn new(table: ChooserTable<T>) -> Self {
        let name = table.current_name().to_owned();
        let index = Arc::new(AtomicCell::new(table.current_index()));
        let table = Arc::new(Mutex::new(table));
        Self { name, table, index }
    }

    fn update_choice(&mut self) {
        let mut table = self.table.lock().unwrap();
        table.choose(self.name.as_str());
        self.index.store(table.current_index());
    }

    fn current_index(&self) -> usize {
        let table = self.table.lock().unwrap();
        table.current_index()
    }
}

struct ReplayerApp {
    midi_scenario: Arc<Mutex<MidiScenario>>,
    midi_in: Arc<Mutex<Option<MidiInput>>>,
    ai_algorithm: TableInfo<Arc<AIFuncType>>,
    human_synth: TableInfo<SynthFunc>,
    ai_synth: TableInfo<SynthFunc>,
    variation_controls: VariationControls,
    replay_delay_slider: Arc<AtomicCell<SliderValue<f64>>>,
    in_port: Option<MidiInputPort>,
    in_port_name: Option<String>,
    melody_pref: Arc<AtomicCell<Preference>>,
    variation_pref: Arc<AtomicCell<Preference>>,
    today_search_pref: Arc<AtomicCell<Preference>>,
    older_search_pref: Arc<AtomicCell<Preference>>,
    melody_var_info: Arc<Mutex<VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>>>,
    melody_var_update_needed: Arc<AtomicCell<bool>>,
    database: Option<Database>,
    input2ai: Arc<SegQueue<SynthMsg>>,
    ai2dbase: Arc<SegQueue<FromAiMsg>>,
    dbase2gui: Arc<SegQueue<DatabaseGuiUpdate>>,
    gui2dbase: Arc<SegQueue<GuiDatabaseUpdate>>,
    gui2ai: Arc<SegQueue<MelodyInfo>>,
    ai2output: Arc<SegQueue<SynthMsg>>,
    melody_progress: Arc<AtomicCell<Option<f32>>>,
    melody_run_status: MelodyRunStatus,
    adjust_search_preferences: bool,
    variations_of_current_melody: bool,
    show_variation: bool,
    show_synth_choices: bool,
    show_melody_sections: bool,
    show_figures: bool,
    new_tags: [String; 2],
    quit_threads: Arc<AtomicCell<bool>>,
}

const MAIN_MELODY_SCALING: f32 = 0.8;
const MIDDLE_C: MidiByte = 60;
const STAFF_PITCH_WIDTH: MidiByte = 19;
const LOWEST_STAFF_PITCH: MidiByte = MIDDLE_C - STAFF_PITCH_WIDTH;
const HIGHEST_STAFF_PITCH: MidiByte = MIDDLE_C + STAFF_PITCH_WIDTH;
const BORDER_SIZE: f32 = 8.0;
const Y_OFFSET: f32 = BORDER_SIZE * 2.0;
const X_OFFSET: f32 = BORDER_SIZE * 5.0;
const ACCIDENTAL_SIZE_MULTIPLIER: f32 = 5.0;
const KEY_SIGNATURE_OFFSET: f32 = 28.0;
const NUM_STAFF_LINES: MidiByte = 5;
const LINE_STROKE: Stroke = Stroke {
    width: 1.0,
    color: Color32::BLACK,
};

macro_rules! load_font {
    ($fonts:ident, $filename:literal) => {{
        let name = $filename
            .split("/")
            .last()
            .unwrap()
            .split(".")
            .next()
            .unwrap()
            .to_owned();
        println!("Loading font {name} from {}.", $filename);
        $fonts.font_data.insert(
            name.clone(),
            FontData::from_static(include_bytes!($filename)),
        );
        $fonts
            .families
            .get_mut(&FontFamily::Proportional)
            .unwrap()
            .push(name);
    }};
}

impl ReplayerApp {
    fn new(cc: &eframe::CreationContext<'_>) -> anyhow::Result<Self> {
        let mut fonts = FontDefinitions::default();
        load_font!(fonts, "../../bravura/BravuraText.otf");
        cc.egui_ctx.set_fonts(fonts);

        let ai_algorithm = TableInfo::new(make_ai_table());
        let human_synth = TableInfo::new(make_synth_table());
        let ai_synth = TableInfo::new(make_synth_table());
        let variation_controls = VariationControls::new();
        let replay_delay_slider = Arc::new(AtomicCell::new(replay_slider()));
        let mut database = Database::new();
        let database_timer = Instant::now();
        let (melody_var_info, melody_pref, variation_pref) =
            Self::wrapped_melody_info(&mut database, Preference::Neutral, Preference::Favorite);
        let database_load_time = database_timer.elapsed().as_secs_f64();
        println!("Database load time: {database_load_time}s");
        let melody_run_status = MelodyRunStatus::new();

        let mut app = ReplayerApp {
            midi_scenario: Arc::new(Mutex::new(MidiScenario::StartingUp)),
            midi_in: Arc::new(Mutex::new(None)),
            variation_controls,
            replay_delay_slider,
            ai_algorithm,
            human_synth,
            ai_synth,
            in_port: None,
            in_port_name: None,
            melody_pref,
            variation_pref,
            today_search_pref: Arc::new(AtomicCell::new(Preference::Neutral)),
            older_search_pref: Arc::new(AtomicCell::new(Preference::Favorite)),
            melody_var_info,
            melody_var_update_needed: Arc::new(AtomicCell::new(true)),
            database: Some(database),
            input2ai: Arc::new(SegQueue::new()),
            ai2dbase: Arc::new(SegQueue::new()),
            dbase2gui: Arc::new(SegQueue::new()),
            gui2dbase: Arc::new(SegQueue::new()),
            gui2ai: Arc::new(SegQueue::new()),
            ai2output: Arc::new(SegQueue::new()),
            melody_progress: Arc::new(AtomicCell::new(None)),
            melody_run_status,
            adjust_search_preferences: false,
            variations_of_current_melody: false,
            show_variation: true,
            show_synth_choices: true,
            show_melody_sections: false,
            show_figures: false,
            new_tags: [String::new(), String::new()],
            quit_threads: Arc::new(AtomicCell::new(false)),
        };
        app.startup();
        Ok(app)
    }

    fn wrapped_melody_info(
        database: &mut Database,
        min_today_pref: Preference,
        min_older_pref: Preference,
    ) -> (
        Arc<Mutex<VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>>>,
        Arc<AtomicCell<Preference>>,
        Arc<AtomicCell<Preference>>,
    ) {
        let (melody_var_info, melody_pref, variation_pref) =
            Self::retrieve_melody_info(database, min_today_pref, min_older_pref);
        (
            Arc::new(Mutex::new(melody_var_info)),
            Arc::new(AtomicCell::new(melody_pref)),
            Arc::new(AtomicCell::new(variation_pref)),
        )
    }

    fn retrieve_melody_info(
        database: &mut Database,
        min_today_pref: Preference,
        min_older_pref: Preference,
    ) -> (
        VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>,
        Preference,
        Preference,
    ) {
        let mut melody_var_info = VecTracker::new(
            database
                .get_melody_pairs(min_today_pref, min_older_pref)
                .unwrap(),
        );
        melody_var_info.go_to_end();
        let (melody_pref, variation_pref) = melody_var_info
            .get()
            .map_or((Preference::Neutral, Preference::Neutral), |(m, v, _)| {
                (m.rating(), v.rating())
            });
        (melody_var_info, melody_pref, variation_pref)
    }

    fn main_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            let heading = format!("Replayer ({})", self.in_port_name.as_ref().unwrap());
            self.control_screen(ui, heading);
        });
    }

    fn control_screen(&mut self, ui: &mut Ui, heading: String) {
        ui.heading(heading);
        ui.checkbox(&mut self.show_synth_choices, "Show Synthesizer Options");
        ui.horizontal(|ui| {
            self.choose_synth_and_variation(ui);
            self.variation_parameter_settings(ui);
        });

        if !self.show_synth_choices {
            self.display_melody_section(ui, MAIN_MELODY_SCALING);
        }
    }

    fn choose_synth_and_variation(&mut self, ui: &mut Ui) {
        ui.horizontal(|ui| {
            let human_name = self.human_synth.name.clone();
            let ai_name = self.ai_synth.name.clone();
            self.show_synths(ui, human_name.as_str(), ai_name.as_str());
            Self::radio_choice(ui, "Variation Algorithm", &mut self.ai_algorithm);
            self.update_synths(human_name.as_str(), ai_name.as_str());
        });
    }

    fn show_synths(&mut self, ui: &mut Ui, human_name: &str, ai_name: &str) {
        if self.show_synth_choices {
            Self::radio_choice(ui, "Human Synthesizer", &mut self.human_synth);
            Self::radio_choice(ui, "Variation Synthesizer", &mut self.ai_synth);
        } else {
            ui.vertical(|ui| {
                ui.label(format!("Human synth: {human_name}"));
                ui.label(format!("AI synth: {ai_name}"));
            });
        }
    }

    fn update_synths(&mut self, human_name: &str, ai_name: &str) {
        if human_name != self.human_synth.name {
            let msg =
                SynthMsg::program_change(self.human_synth.current_index() as u8, HUMAN_SPEAKER);
            self.ai2output.push(msg);
        }
        if ai_name != self.ai_synth.name {
            let msg =
                SynthMsg::program_change(self.ai_synth.current_index() as u8, VARIATION_SPEAKER);
            self.ai2output.push(msg);
        }
    }

    fn variation_parameter_settings(&mut self, ui: &mut Ui) {
        ui.vertical(|ui| {
            ui.label("Variation Algorithm Controls");
            let p_random_slider = self.variation_controls.p_random_slider.clone();
            Self::insert_slider(ui, p_random_slider, "Probability of Randomization");
            let p_ornament_slider = self.variation_controls.p_ornament_slider.clone();
            Self::insert_slider(ui, p_ornament_slider, "Probability of Inserting Ornament");
            let replay_delay_slider = self.replay_delay_slider.clone();
            Self::insert_slider(ui, replay_delay_slider, "Replay Delay (seconds)");
            let shortest_note_slider = self.variation_controls.shortest_note_slider.clone();
            Self::insert_slider(ui, shortest_note_slider, "Shortest Playable Note (seconds)");
            let mut whimsify = self.variation_controls.whimsify.load();
            ui.checkbox(&mut whimsify, "Whimsify Suffix");
            self.variation_controls.whimsify.store(whimsify);
        });
    }

    fn display_melody_section(&mut self, ui: &mut Ui, staff_scaling: f32) {
        ui.checkbox(
            &mut self.adjust_search_preferences,
            "Set Search Preferences",
        );
        if self.adjust_search_preferences {
            self.search_preference_screen(ui);
        } else {
            ui.checkbox(&mut self.show_melody_sections, "Show Melody Sections");
            ui.checkbox(&mut self.show_figures, "Show Figure Boundaries");
            let before = self.variations_of_current_melody;
            ui.checkbox(
                &mut self.variations_of_current_melody,
                "Variations of Current Melody",
            );
            if self.variations_of_current_melody {
                if !before {
                    self.request_refresh();
                }
            } else {
                if before {
                    self.request_refresh();
                }
                ui.checkbox(&mut self.show_variation, "Show Variation");
            }
            if self.displaying_melody_var_info() {
                self.display_melody_info(ui, staff_scaling);
            }
        }
    }

    fn displaying_melody_var_info(&self) -> bool {
        let melody_var_info = self.melody_var_info.lock().unwrap();
        !melody_var_info.is_empty()
    }

    fn search_preference_screen(&self, ui: &mut Ui) {
        let old_today = self.today_search_pref.load();
        let old_older = self.older_search_pref.load();
        ui.label("Minimum Preference for Today");
        Self::preference_buttons(ui, self.today_search_pref.clone());
        ui.label("Minimum Preference for Previous Days");
        Self::preference_buttons(ui, self.older_search_pref.clone());
        if old_today != self.today_search_pref.load() || old_older != self.older_search_pref.load()
        {
            self.request_refresh();
        }
    }

    fn request_refresh(&self) {
        if self.variations_of_current_melody {
            let melody_var_info = self.melody_var_info.lock().unwrap();
            if let Some((info, _, _)) = melody_var_info.get() {
                self.gui2dbase
                    .push(GuiDatabaseUpdate::VariationsOf(info.row_id()));
            }
        } else {
            let min_today_pref = self.today_search_pref.load();
            let min_older_pref = self.older_search_pref.load();
            let refresh = if self.show_variation {
                GuiDatabaseUpdate::RefreshAllPairs {
                    min_today_pref,
                    min_older_pref,
                }
            } else {
                GuiDatabaseUpdate::RefreshAllMelodies {
                    min_today_pref,
                    min_older_pref,
                }
            };
            self.gui2dbase.push(refresh);
        }
    }

    fn display_melody_info(&mut self, ui: &mut Ui, staff_scaling: f32) {
        let (melody_info, variation_info, stats) = {
            let mut melody_var_info = self.melody_var_info.lock().unwrap();
            self.melody_variation_selector(ui, &mut melody_var_info);
            melody_var_info.get().cloned().unwrap()
        };
        if self.melody_var_update_needed.load() {
            self.variation_controls.update_from(&stats);
            self.ai_algorithm.name = stats.algorithm_name;
            self.ai_algorithm.update_choice();
            self.melody_var_update_needed.store(false);
        }

        self.show_pref_selector(ui, "Melody", self.melody_pref.clone());
        self.tags(ui, &melody_info, 0);
        if self.show_variation {
            self.show_pref_selector(ui, "Variation", self.variation_pref.clone());
            self.tags(ui, &variation_info, 1);
        }

        ui.horizontal(|ui| {
            self.melody_play_stop_buttons(ui, &melody_info, &variation_info);
        });

        if ui.button("Create New Variation").clicked() {
            self.create_new_variation(&melody_info);
        }

        let size = Vec2::new(ui.available_width(), ui.available_height() * staff_scaling);
        let mut melodies = vec![(melody_info.melody(), Color32::BLACK)];
        if self.show_variation {
            melodies.push((variation_info.melody(), Color32::RED));
        }
        MelodyRenderer::render(
            ui,
            size,
            &melodies,
            self.show_melody_sections,
            self.show_figures,
            self.melody_progress.clone(),
        );
    }

    fn show_pref_selector(&mut self, ui: &mut Ui, label: &str, pref: Arc<AtomicCell<Preference>>) {
        ui.horizontal(|ui| {
            ui.label(format!("{label} Preference"));
            self.select_pref(ui, pref);
        });
    }

    fn tags(&mut self, ui: &mut Ui, info: &MelodyInfo, new_tag_index: usize) {
        ui.horizontal(|ui| {
            for tag in info.tags().iter() {
                ui.label(format!("#{tag}"));
            }
            ui.label("New tag");
            let response = ui.add(TextEdit::singleline(&mut self.new_tags[new_tag_index]));
            if response.lost_focus() && ui.input(|i| i.key_pressed(Key::Enter)) {
                if self.new_tags[new_tag_index].starts_with('#') {
                    self.new_tags[new_tag_index] = self.new_tags[new_tag_index][1..].to_owned();
                }
                self.gui2dbase.push(GuiDatabaseUpdate::NewTag {
                    rowid: info.row_id(),
                    tag: self.new_tags[new_tag_index].clone(),
                });
                self.new_tags[new_tag_index] = String::new();
                self.request_refresh();
            }
        });
    }

    fn select_pref(&mut self, ui: &mut Ui, pref: Arc<AtomicCell<Preference>>) {
        let start_pref = pref.load();
        Self::preference_buttons(ui, pref.clone());
        if start_pref != pref.load() {
            self.melody_run_status.send_stop();
            let mut melody_var_info = self.melody_var_info.lock().unwrap();
            self.update_database_preferences(&mut melody_var_info);
            self.request_refresh();
        }
    }

    fn melody_play_stop_buttons(
        &self,
        ui: &mut Ui,
        melody_info: &MelodyInfo,
        variation_info: &MelodyInfo,
    ) {
        self.melody_buttons(ui, &melody_info, SynthChoice::Original);
        if self.show_variation {
            self.melody_buttons(ui, &variation_info, SynthChoice::Variation);
            if ui.button("Play Both").clicked() {
                self.play_both(melody_info, variation_info);
            }
        }
        if self.melody_progress.load().is_some() {
            if ui.button("Stop").clicked() {
                self.melody_run_status.send_stop();
            }
        }
    }

    fn melody_variation_selector(
        &self,
        ui: &mut Ui,
        melody_var_info: &mut VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>,
    ) {
        ui.horizontal(|ui| {
            self.melody_arrow(
                "<",
                ui,
                melody_var_info,
                |mvi| mvi.at_start(),
                |mvi| mvi.go_left(),
            );

            if let Some((melody_info, variation_info, _)) = melody_var_info.get() {
                let info_choice = if self.show_variation {
                    variation_info
                } else {
                    melody_info
                };
                ui.label(info_choice.date_time_stamp());
                ui.label(info_choice.scale_name());
            }

            self.melody_arrow(
                ">",
                ui,
                melody_var_info,
                |mvi| mvi.at_end(),
                |mvi| mvi.go_right(),
            );
        });
    }

    fn melody_arrow<
        F: Fn(&VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>) -> bool,
        M: FnMut(&mut VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>),
    >(
        &self,
        arrow: &str,
        ui: &mut Ui,
        melody_var_info: &mut VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>,
        filter: F,
        mut mover: M,
    ) {
        if !filter(melody_var_info) && ui.button(arrow).clicked() {
            self.melody_run_status.send_stop();
            mover(melody_var_info);
            let (melody_info, variation_info, _) = melody_var_info.get().unwrap();
            self.melody_pref.store(melody_info.rating());
            self.variation_pref.store(variation_info.rating());
            self.melody_var_update_needed.store(true);
        }
    }

    fn update_database_preferences(
        &self,
        melody_var_info: &mut VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>,
    ) {
        {
            let (m, v, _) = melody_var_info.get_mut().unwrap();
            m.set_rating(self.melody_pref.load());
            v.set_rating(self.variation_pref.load());
        }
        let (m, v, _) = melody_var_info.get().unwrap();
        self.gui2dbase.push(m.update_preference());
        self.gui2dbase.push(v.update_preference());
    }

    fn create_new_variation(&mut self, melody_info: &MelodyInfo) {
        {
            let mut ai_table = self.ai_algorithm.table.lock().unwrap();
            if ai_table.current_name() == NO_AI_NAME {
                ai_table.choose(DEFAULT_AI_NAME);
                self.ai_algorithm.name = String::from(DEFAULT_AI_NAME);
            }
        }
        self.gui2ai.push(melody_info.clone());
    }

    fn play_both(&self, melody_info: &MelodyInfo, variation_info: &MelodyInfo) {
        self.melody_run_status.send_stop();
        let human_melody = melody_info.melody().clone();
        let computer_melody = variation_info.melody().clone();
        let ai2output = self.ai2output.clone();
        let melody_progress = self.melody_progress.clone();
        let melody_run_status = self.melody_run_status.clone();
        thread::spawn(move || {
            while melody_run_status.is_stopping() {}
            send_two_melodies(
                &human_melody,
                &computer_melody,
                ai2output,
                melody_progress,
                melody_run_status,
            );
        });
    }

    fn font_id(size: f32) -> FontId {
        FontId {
            size,
            family: FontFamily::Proportional,
        }
    }

    fn melody_buttons(&self, ui: &mut Ui, info: &MelodyInfo, synth: SynthChoice) {
        let text = format!("Play {synth:?}");
        if ui.button(text).clicked() {
            self.melody_run_status.send_stop();
            self.play_melody_thread(info.melody().clone(), synth.speaker());
        }
    }

    fn play_melody_thread(&self, melody: Melody, speaker: Speaker) {
        let ai2output = self.ai2output.clone();
        let melody_progress = self.melody_progress.clone();
        let melody_run_status = self.melody_run_status.clone();
        thread::spawn(move || {
            while melody_run_status.is_stopping() {}
            send_recorded_melody(
                &melody,
                speaker,
                ai2output,
                melody_progress,
                melody_run_status,
            );
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

    fn radio_choice<T: Clone>(ui: &mut Ui, header: &str, info: &mut TableInfo<T>) {
        ui.vertical(|ui| {
            let table = info.table.lock().unwrap();
            ui.label(header);
            for item in table.name_vec() {
                ui.radio_value(&mut info.name, item.clone(), item.clone());
            }
        });
        info.update_choice();
    }

    fn insert_slider<N: FromStr + Numeric + Display>(
        ui: &mut Ui,
        slider: Arc<AtomicCell<SliderValue<N>>>,
        text: &str,
    ) {
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

    fn startup(&mut self) {
        start_output_thread::<NUM_OUTPUT_CHANNELS>(
            self.ai2output.clone(),
            {
                let table = self.human_synth.table.lock().unwrap();
                Arc::new(Mutex::new(table.choice_vec()))
            },
            self.quit_threads.clone(),
        );
        start_ai_thread(
            self.ai_algorithm.table.clone(),
            self.input2ai.clone(),
            self.gui2ai.clone(),
            self.ai2output.clone(),
            self.ai2dbase.clone(),
            self.variation_controls.clone(),
            self.replay_delay_slider.clone(),
            self.melody_progress.clone(),
            self.melody_run_status.clone(),
        );

        let database = self.database.take();

        start_database_thread(
            self.dbase2gui.clone(),
            self.gui2dbase.clone(),
            self.ai2dbase.clone(),
            database.unwrap(),
        );

        self.try_midi_input();
    }

    fn try_midi_input(&self) {
        let mut midi_in = MidiInput::new("midir reading input");
        let scenario = MidiScenario::new(&mut midi_in);
        {
            let mut midi_scenario = self.midi_scenario.lock().unwrap();
            *midi_scenario = scenario;
        }
        let mut midi_in_record = self.midi_in.lock().unwrap();
        *midi_in_record = midi_in.ok();
    }

    fn no_midi_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame, message: &str) {
        egui::CentralPanel::default().show(ctx, |ui| {
            self.control_screen(ui, message.to_owned());
            self.start_ui_listening_thread(ctx);
        });
    }

    fn pick_midi_screen(
        &mut self,
        ctx: &egui::Context,
        _frame: &mut eframe::Frame,
        in_ports: &MidiInputPorts,
    ) {
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
                self.start_input(ctx);
            }
        });
    }

    fn start_input(&mut self, ctx: &egui::Context) {
        let in_port = self.in_port.as_ref().unwrap().clone();
        self.set_in_port_name(&in_port);
        let midi_in = {
            let mut self_midi_in = self.midi_in.lock().unwrap();
            self_midi_in.take()
        };

        self.start_ui_listening_thread(ctx);

        start_input_thread(
            self.input2ai.clone(),
            midi_in.unwrap(),
            self.in_port.as_ref().unwrap().clone(),
            self.quit_threads.clone(),
        );
    }

    fn start_ui_listening_thread(&self, ctx: &egui::Context) {
        let ctx = ctx.clone();
        let dbase2gui = self.dbase2gui.clone();
        let melody_pref = self.melody_pref.clone();
        let variation_pref = self.variation_pref.clone();
        let melody_var_info = self.melody_var_info.clone();
        let melody_progress = self.melody_progress.clone();
        let update_needed = self.melody_var_update_needed.clone();
        thread::spawn(move || loop {
            if let Some(msg) = dbase2gui.pop() {
                Self::handle_database_msg(
                    msg,
                    melody_pref.clone(),
                    variation_pref.clone(),
                    melody_var_info.clone(),
                );
                update_needed.store(true);
                ctx.request_repaint();
            }
            if let Some(_) = melody_progress.load() {
                ctx.request_repaint();
            }
            thread::sleep(Duration::from_millis(25));
        });
    }

    fn handle_database_msg(
        msg: DatabaseGuiUpdate,
        melody_pref: Arc<AtomicCell<Preference>>,
        variation_pref: Arc<AtomicCell<Preference>>,
        melody_var_info: Arc<Mutex<VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>>>,
    ) {
        match msg {
            DatabaseGuiUpdate::Info {
                melody: player_info,
                variation: variation_info,
                stats,
            } => {
                melody_pref.store(player_info.rating());
                variation_pref.store(variation_info.rating());
                let mut melody_var_info = melody_var_info.lock().unwrap();
                melody_var_info.add((player_info, variation_info, stats));
            }
            DatabaseGuiUpdate::AllPairs(pairs) => {
                let mut melody_var_info = melody_var_info.lock().unwrap();
                Self::update_melody_var_info(&mut melody_var_info, pairs);
                if let Some((_, v, _)) = melody_var_info.get() {
                    variation_pref.store(v.rating());
                }
            }
            DatabaseGuiUpdate::Melodies(melodies) => {
                let mut melody_var_info = melody_var_info.lock().unwrap();
                let stats = melody_var_info.get().map_or(
                    VariationControls::new().stats(NO_AI_NAME.to_owned()),
                    |(_, _, stats)| stats.clone(),
                );
                let replacement = melodies
                    .iter()
                    .map(|m| (m.clone(), m.clone(), stats.clone()))
                    .collect();
                Self::update_melody_var_info(&mut melody_var_info, replacement);
                if let Some((m, _, _)) = melody_var_info.get() {
                    melody_pref.store(m.rating());
                }
            }
        }
    }

    fn update_melody_var_info(
        melody_var_info: &mut VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>,
        replacement: Vec<(MelodyInfo, MelodyInfo, VariationStats)>,
    ) {
        let current_timestamp = melody_var_info.get().map(|(m, _, _)| m.timestamp());
        melody_var_info.replace_vec(replacement);
        if let Some(current_timestamp) = current_timestamp {
            Self::align_to_timestamp(current_timestamp, melody_var_info);
        }
    }

    fn align_to_timestamp(
        current_timestamp: i64,
        melody_var_info: &mut VecTracker<(MelodyInfo, MelodyInfo, VariationStats)>,
    ) {
        while !melody_var_info.at_start()
            && melody_var_info
                .get()
                .map_or(false, |(m, _, _)| m.timestamp() > current_timestamp)
        {
            melody_var_info.go_left();
        }
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
    y_range: RangeInclusive<f32>,
    y_per_pitch: f32,
    y_middle_c: f32,
    hi: MidiByte,
    melody_progress: Arc<AtomicCell<Option<f32>>>,
}

impl MelodyRenderer {
    fn staff_line_space(&self) -> f32 {
        self.y_per_pitch * 2.0
    }

    fn space_above_staff(&self) -> f32 {
        1.0 + self
            .scale
            .diatonic_steps_between_round_up(HIGHEST_STAFF_PITCH, self.hi) as f32
    }

    fn min_x(&self) -> f32 {
        *self.x_range.start()
    }

    fn total_note_x(&self) -> f32 {
        *self.x_range.end() - self.note_offset_x()
    }

    fn note_offset_x(&self) -> f32 {
        self.min_x() + X_OFFSET + KEY_SIGNATURE_OFFSET + self.y_per_pitch * self.sig.len() as f32
    }

    fn render(
        ui: &mut Ui,
        size: Vec2,
        melodies: &Vec<(&Melody, Color32)>,
        show_sections: bool,
        show_figures: bool,
        melody_progress: Arc<AtomicCell<Option<f32>>>,
    ) {
        if melodies.len() > 0 {
            let (response, painter) = ui.allocate_painter(size, Sense::hover());
            let scale = melodies[0].0.best_scale_for();
            let (lo, hi) = Self::min_max_staff(&scale, melodies);
            let num_diatonic_pitches =
                1 + scale.diatonic_steps_between(lo, hi).pure_degree().unwrap();
            let y_per_pitch = ((response.rect.max.y - response.rect.min.y) - BORDER_SIZE * 2.0)
                / num_diatonic_pitches as f32;
            let y_border = Y_OFFSET + response.rect.min.y;
            let renderer = MelodyRenderer {
                hi,
                scale,
                y_per_pitch,
                x_range: response.rect.min.x + BORDER_SIZE..=response.rect.max.x - BORDER_SIZE,
                y_range: response.rect.min.y + BORDER_SIZE..=response.rect.max.y - BORDER_SIZE,
                sig: scale.key_signature(),
                y_middle_c: y_border
                    + y_per_pitch * scale.diatonic_steps_between_round_up(MIDDLE_C, hi) as f32,
                melody_progress,
            };
            renderer.draw_progress(&painter);
            let y_treble = y_border + y_per_pitch * renderer.space_above_staff();
            renderer.draw_staff(&painter, Clef::Treble, y_treble);
            let y_bass = renderer.y_middle_c + renderer.staff_line_space();
            renderer.draw_staff(&painter, Clef::Bass, y_bass);
            let mut first_melody = true;
            for (melody, color) in melodies.iter().rev() {
                renderer.draw_melody(&painter, melody, show_sections, show_figures && first_melody, *color);
                first_melody = false;
            }
        }
    }

    fn draw_progress(&self, painter: &Painter) {
        if let Some(progress) = self.melody_progress.load() {
            let x = self.note_offset_x() + self.total_note_x() * progress;
            let y1 = *self.y_range.start();
            let y2 = *self.y_range.end();
            painter.line_segment(
                [Pos2 { x, y: y1 }, Pos2 { x, y: y2 }],
                Stroke {
                    width: 5.0,
                    color: Color32::GREEN,
                },
            );
        }
    }

    fn draw_melody(
        &self,
        painter: &Painter,
        melody: &Melody,
        show_sections: bool,
        show_figures: bool,
        color: Color32,
    ) {
        let mut note_renderer =
            IncrementalNoteRenderer::new(self, painter, melody, show_sections, show_figures, color);
        for (i, note) in melody.iter().enumerate() {
            let x = self.note_offset_x()
                + self.total_note_x() * note_renderer.total_duration / melody.duration() as f32;
            note_renderer.note_update(note, &self.scale);
            let y = self.y_middle_c - note_renderer.staff_offset as f32 * self.y_per_pitch;
            if !note.is_rest() {
                note_renderer.show_note(i, x, y);
            }
            if note_renderer.can_show_figures() {
                note_renderer.show_figures(i, x, y);
            }
        }
    }

    fn draw_staff(&self, painter: &Painter, clef: Clef, start_y: f32) {
        let mut y = start_y;
        clef.render(painter, self.min_x(), y, self.y_per_pitch);
        for _ in 0..NUM_STAFF_LINES {
            painter.hline(self.x_range.clone(), y, LINE_STROKE);
            y += self.staff_line_space();
        }
        for (i, position) in clef.key_signature_positions(&self.sig).iter().enumerate() {
            let x = self.min_x() + KEY_SIGNATURE_OFFSET + self.y_per_pitch * i as f32;
            let y = self.y_middle_c - *position as f32 * self.y_per_pitch;
            self.draw_accidental(painter, self.sig.symbol(), x, y, Color32::BLACK);
        }
    }

    fn draw_accidental(
        &self,
        painter: &Painter,
        text: Accidental,
        x: f32,
        y: f32,
        text_color: Color32,
    ) {
        painter.text(
            Pos2 { x, y },
            Align2::CENTER_CENTER,
            text.symbol(),
            ReplayerApp::font_id(ACCIDENTAL_SIZE_MULTIPLIER * self.y_per_pitch),
            text_color,
        );
    }

    fn draw_extra_dashes(&self, painter: &Painter, x: f32, staff_offset: MidiByte) {
        let staff_extra_threshold = (NUM_STAFF_LINES + 1) * 2;
        if staff_offset == 0 {
            self.draw_extra_dash(painter, x, staff_offset);
        } else if staff_offset >= staff_extra_threshold {
            for offset in staff_extra_threshold..=staff_offset {
                self.draw_extra_dash(painter, x, offset);
            }
        } else if staff_offset <= -staff_extra_threshold {
            for offset in staff_offset..=-staff_extra_threshold {
                self.draw_extra_dash(painter, x, offset);
            }
        }
    }

    fn draw_extra_dash(&self, painter: &Painter, x: f32, staff_offset: MidiByte) {
        let x_offset = self.y_per_pitch * 1.5;
        let x1 = x - x_offset;
        let x2 = x + x_offset;
        let y = self.y_middle_c - staff_offset as f32 * self.y_per_pitch;
        painter.line_segment([Pos2 { x: x1, y }, Pos2 { x: x2, y }], LINE_STROKE);
    }

    fn min_max_staff(
        scale: &MusicMode,
        melodies: &Vec<(&Melody, Color32)>,
    ) -> (MidiByte, MidiByte) {
        let mut lo = LOWEST_STAFF_PITCH;
        let mut hi = HIGHEST_STAFF_PITCH;
        for (melody, _) in melodies.iter() {
            let (mlo, mhi) = melody.min_max_pitches();
            lo = min(lo, mlo);
            hi = max(hi, mhi);
        }
        (scale.closest_pitch_below(lo), scale.closest_pitch_above(hi))
    }
}

static FIGURE_COLORS: [Color32; 8] = [Color32::DARK_GREEN, Color32::DARK_RED, Color32::DARK_BLUE, Color32::GOLD, Color32::GREEN, Color32::RED, Color32::BLUE, Color32::BROWN];

struct ColorMaker {
    next: ModNum<usize>
}

impl ColorMaker {
    fn new() -> Self {
        Self {next: ModNum::new(0, FIGURE_COLORS.len())}
    }

    fn next(&mut self) -> Color32 {
        let result = FIGURE_COLORS[self.next.a()];
        self.next += 1;
        result
    }
}


#[derive(Copy, Clone, PartialEq, Debug)]
struct PendingFigureBox {
    color: Color32,
    x: f32,
    y_min: f32,
    y_max: f32,
    last_note: usize,
}

impl PendingFigureBox {
    fn new(color: Color32, x: f32, y: f32, last_note: usize) -> Self {
        Self {
            color,
            x,
            y_min: y,
            y_max: y,
            last_note,
        }
    }

    fn active(&self, i: usize) -> bool {
        i <= self.last_note
    }

    fn update_y(&mut self, y: f32) {
        if y < self.y_min {
            self.y_min = y;
        }
        if y > self.y_max {
            self.y_max = y;
        }
    }
}

struct IncrementalNoteRenderer<'a> {
    renderer: &'a MelodyRenderer,
    melody: &'a Melody,
    painter: &'a Painter,
    total_duration: f32,
    figure_boundaries: VecDeque<(usize, usize, MelodicFigure)>,
    figure_colors: HashMap<MelodicFigure, Color32>,
    show_sections: bool,
    show_figures: bool,
    staff_offset: i16,
    note_color: Color32,
    auxiliary_symbol: Option<Accidental>,
    pending_figures: Vec<PendingFigureBox>,
}

impl<'a> IncrementalNoteRenderer<'a> {
    fn new(
        renderer: &'a MelodyRenderer,
        painter: &'a Painter,
        melody: &'a Melody,
        show_sections: bool,
        show_figures: bool,
        note_color: Color32,
    ) -> Self {
        let figure_boundaries = melody.figure_boundaries();
        let mut figure_colors = HashMap::new();
        let mut colors = ColorMaker::new();
        for (_, _, f) in figure_boundaries.iter() {
            if !figure_colors.contains_key(f) {
                figure_colors.insert(*f, colors.next());
            }
        }
        Self {
            renderer,
            total_duration: 0.0,
            melody,
            painter,
            show_figures,
            show_sections,
            figure_boundaries,
            figure_colors,
            auxiliary_symbol: None,
            staff_offset: 0,
            note_color,
            pending_figures: vec![],
        }
    }

    fn note_update(&mut self, note: &Note, scale: &MusicMode) {
        self.total_duration += note.duration() as f32;
        let (staff_offset, auxiliary_symbol) = scale.staff_position(note.pitch());
        self.staff_offset = staff_offset;
        self.auxiliary_symbol = auxiliary_symbol;
    }

    fn show_note(&self, i: usize, x: f32, y: f32) {
        if self.show_sections {
            self.show_sections(i, x, y);
        } else {
            self.painter
                .circle_filled(Pos2 { x, y }, self.renderer.y_per_pitch, self.note_color);
        }
        if let Some(auxiliary_symbol) = self.auxiliary_symbol {
            let x = x + self.renderer.staff_line_space();
            self.renderer
                .draw_accidental(self.painter, auxiliary_symbol, x, y, self.note_color);
        }
        self.renderer
            .draw_extra_dashes(self.painter, x, self.staff_offset);
    }

    fn show_sections(&self, i: usize, x: f32, y: f32) {
        match self.melody.section_number_for(i) {
            None => self.painter.circle_filled(
                Pos2 { x, y },
                self.renderer.y_per_pitch,
                self.note_color,
            ),
            Some(s) => {
                self.painter.text(
                    Pos2 { x, y },
                    Align2::CENTER_CENTER,
                    format!("{s}"),
                    ReplayerApp::font_id(ACCIDENTAL_SIZE_MULTIPLIER * self.renderer.y_per_pitch),
                    self.note_color,
                );
            }
        };
    }

    fn can_show_figures(&self) -> bool {
        self.show_figures && self.figure_boundaries.len() > 0
    }

    fn show_figures(&mut self, i: usize, x: f32, y: f32) {
        self.set_up_figures(i, x, y);
        self.update_active_figures(i, y);
        self.resolve_completed_figures(i, x);
    }

    fn set_up_figures(&mut self, i: usize, x: f32, y: f32) {
        for (start, end, figure) in self.figure_boundaries.iter() {
            if i == *start {
                self.pending_figures.push(PendingFigureBox::new(self.figure_colors.get(figure).copied().unwrap(), x, y, *end));
            }
        }
    }

    fn update_active_figures(&mut self, i: usize, y: f32) {
        for pending in self.pending_figures.iter_mut() {
            if pending.active(i) {
                pending.update_y(y);
            }
        }
    }

    fn resolve_completed_figures(&self, i: usize, x: f32) {
        for pending in self.pending_figures.iter() {
            if pending.last_note == i {
                let rect = Rect::from_min_max(
                    Pos2::new(
                        pending.x - self.renderer.y_per_pitch,
                        pending.y_min - self.renderer.y_per_pitch,
                    ),
                    Pos2::new(
                        x + self.renderer.y_per_pitch,
                        pending.y_max + self.renderer.y_per_pitch,
                    ),
                );
                self.painter
                    .rect_stroke(rect, 0.0, egui::Stroke::new(1.0, pending.color));
            }
        }
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
                    self.start_input(ctx);
                }
                self.main_screen(ctx, frame)
            }
            MidiScenario::MultipleInputPorts { in_ports } => {
                self.pick_midi_screen(ctx, frame, &in_ports);
            }
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Clef {
    Treble,
    Bass,
}

impl Clef {
    pub fn symbol(&self) -> char {
        match self {
            Self::Treble => '\u{1d11e}',
            Self::Bass => '\u{1d122}',
        }
    }

    pub fn key_signature_positions(&self, sig: &KeySignature) -> Vec<MidiByte> {
        match self {
            Self::Treble => sig.treble_clef(),
            Self::Bass => sig.bass_clef(),
        }
    }

    fn size(&self) -> f32 {
        match self {
            Self::Treble => 13.5,
            Self::Bass => 8.0,
        }
    }

    fn x_offset(&self) -> f32 {
        10.0
    }

    fn y_offset(&self) -> f32 {
        match self {
            Self::Treble => 5.0,
            Self::Bass => -0.45,
        }
    }

    fn render(&self, painter: &Painter, x: f32, y: f32, y_per_pitch: f32) {
        painter.text(
            Pos2 {
                x: x + self.x_offset(),
                y: y + self.y_offset() * y_per_pitch,
            },
            Align2::CENTER_CENTER,
            self.symbol(),
            ReplayerApp::font_id(self.size() * y_per_pitch),
            Color32::BLACK,
        );
    }
}
