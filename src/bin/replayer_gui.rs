use eframe::egui::Ui;
use crossbeam_queue::SegQueue;
use eframe::egui;
use eframe::emath::Numeric;
use midir::{Ignore, MidiInput, MidiInputPort, MidiInputPorts};
use musicserver1::{make_ai_table, make_synth_table, prob_slider, ornament_gap_slider, replay_slider,
                   start_ai_thread, start_input_thread, start_output_thread, AITable, ChooserTable,
                   SliderValue, SynthTable, Preference, MelodyInfo, Database, start_database_thread,
                   SynthChoice, send_recorded_melody, GuiDatabaseUpdate, Melody};
use std::{mem, thread};
use std::sync::{Arc, Mutex};
use enum_iterator::all;
use bare_metal_modulo::*;
use std::str::FromStr;
use crossbeam_utils::atomic::AtomicCell;
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
    p_random_slider: Arc<AtomicCell<SliderValue<f64>>>,
    p_ornament_slider: Arc<AtomicCell<SliderValue<f64>>>,
    ornament_gap_slider: Arc<AtomicCell<SliderValue<i64>>>,
    replay_delay_slider: Arc<AtomicCell<SliderValue<f64>>>,
    in_port: Option<MidiInputPort>,
    in_port_name: Option<String>,
    melody_pref: Arc<Mutex<Preference>>,
    variation_pref: Arc<Mutex<Preference>>,
    melody_var_info: Arc<Mutex<VecTracker<(MelodyInfo, MelodyInfo)>>>,
    database: Option<Database>,
    dbase2gui: Arc<SegQueue<(MelodyInfo,MelodyInfo)>>,
    gui2dbase: Arc<SegQueue<GuiDatabaseUpdate>>,
    ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>
}

impl ReplayerApp {
    fn new(_cc: &eframe::CreationContext<'_>) -> anyhow::Result<Self> {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_visuals.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.

        let ai_table = make_ai_table();
        let human_synth_table = make_synth_table();
        let ai_synth_table = make_synth_table();
        let ai_name = ai_table.current_name().to_string();
        let human_synth_name = human_synth_table.current_name().to_string();
        let ai_synth_name = ai_synth_table.current_name().to_string();
        let ai_table = Arc::new(Mutex::new(ai_table));
        let human_synth_table = Arc::new(Mutex::new(human_synth_table));
        let ai_synth_table = Arc::new(Mutex::new(ai_synth_table));
        let p_random_slider = Arc::new(AtomicCell::new(prob_slider()));
        let p_ornament_slider = Arc::new(AtomicCell::new(prob_slider()));
        let ornament_gap_slider = Arc::new(AtomicCell::new(ornament_gap_slider()));
        let replay_delay_slider = Arc::new(AtomicCell::new(replay_slider()));
        let mut database = Database::new();
        let melody_var_info = Arc::new(Mutex::new({
            let mut melody_var_info = VecTracker::new(database.get_melody_pairs().unwrap());
            melody_var_info.go_to_end();
            melody_var_info
        }));

        let app = ReplayerApp {
            midi_scenario: Arc::new(Mutex::new(MidiScenario::StartingUp)),
            midi_in: Arc::new(Mutex::new(None)),
            p_random_slider,
            p_ornament_slider,
            ornament_gap_slider,
            replay_delay_slider,
            ai_table,
            human_synth_table,
            ai_synth_name,
            ai_synth_table,
            ai_name,
            human_synth_name,
            in_port: None,
            in_port_name: None,
            melody_pref: Arc::new(Mutex::new(Preference::Neutral)),
            variation_pref: Arc::new(Mutex::new(Preference::Neutral)),
            melody_var_info,
            database: Some(database),
            dbase2gui: Arc::new(SegQueue::new()),
            gui2dbase: Arc::new(SegQueue::new()),
            ai2output: Arc::new(SegQueue::new())
        };
        app.startup_thread();
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

            Self::insert_slider(ui,self.p_random_slider.clone(),"Probability of Randomization");
            Self::insert_slider(ui,self.p_random_slider.clone(),"Probability of Inserting Ornament");
            Self::insert_slider(ui, self.ornament_gap_slider.clone(), "Notes Between Ornaments");
            Self::insert_slider(ui, self.replay_delay_slider.clone(), "Replay Delay");
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
        let mut melody_pref = self.melody_pref.lock().unwrap();
        let mut variation_pref = self.variation_pref.lock().unwrap();
        let (melody_info, variation_info) = melody_var_info.get().cloned().unwrap();

        ui.horizontal(|ui| {
            if !melody_var_info.at_start() && ui.button("<").clicked() {
                melody_var_info.go_left();
                let (melody_info, variation_info) = melody_var_info.get().unwrap();
                *melody_pref = melody_info.get_rating();
                *variation_pref = variation_info.get_rating();
            }

            let start_melody_pref = *melody_pref;
            let start_variation_pref = *variation_pref;
            ui.vertical(|ui| {
                Self::melody_buttons(self.ai2output.clone(), ui, &melody_info, &mut melody_pref, SynthChoice::Human);
                Self::melody_buttons(self.ai2output.clone(), ui, &variation_info, &mut variation_pref, SynthChoice::Ai);
            });
            if !melody_var_info.at_end() && ui.button(">").clicked() {
                melody_var_info.go_right();
                let (melody_info, variation_info) = melody_var_info.get().unwrap();
                *melody_pref = melody_info.get_rating();
                *variation_pref = variation_info.get_rating();
            }

            if start_melody_pref != *melody_pref {
                melody_var_info.get_mut().unwrap().0.set_rating(*melody_pref);
                self.gui2dbase.push(melody_var_info.get().cloned().unwrap().0.get_update());
            }
            if start_variation_pref != *variation_pref {
                melody_var_info.get_mut().unwrap().1.set_rating(*variation_pref);
                self.gui2dbase.push(melody_var_info.get().cloned().unwrap().1.get_update());
            }
        });
    }

    fn melody_buttons(ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>, ui: &mut Ui, info: &MelodyInfo, pref: &mut Preference, synth: SynthChoice) {
        ui.horizontal(|ui| {
            ui.label(info.date_time_stamp());
            Self::preference_buttons(ui, pref);
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

    fn preference_buttons(ui: &mut Ui, pref: &mut Preference) {
        ui.horizontal(|ui| {
            for preference in all::<Preference>() {
                ui.radio_value(pref, preference, preference.to_string());
            }
        });
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
        slider.store(sv.slid(value));
    }

    fn startup_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Replayer: Looking for MIDI devices...");
        });
    }

    fn startup_thread(&self) {
        let midi_in_record = self.midi_in.clone();
        let midi_scenario = self.midi_scenario.clone();
        std::thread::spawn(move || {
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
                let mut midi_scenario = midi_scenario.lock().unwrap();
                *midi_scenario = scenario;
            }
            let mut midi_in_record = midi_in_record.lock().unwrap();
            *midi_in_record = midi_in.ok();
        });
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
            self.replay_delay_slider.clone(),
            self.ornament_gap_slider.clone(),
            self.p_random_slider.clone(),
            self.p_ornament_slider.clone(),
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
        let melody_pref = self.melody_pref.clone();
        let variation_pref = self.variation_pref.clone();
        let melody_var_info = self.melody_var_info.clone();
        thread::spawn(move || {
            loop {
                if let Some((player_info, variation_info)) = dbase2gui.pop() {
                    let mut melody_pref = melody_pref.lock().unwrap();
                    *melody_pref = player_info.get_rating();
                    let mut variation_pref = variation_pref.lock().unwrap();
                    *variation_pref = variation_info.get_rating();
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

impl eframe::App for ReplayerApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
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