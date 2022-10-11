use crate::egui::Ui;
use crossbeam_queue::SegQueue;
use eframe::egui;
use midir::{Ignore, MidiInput, MidiInputPort, MidiInputPorts};
use musicserver1::{make_ai_table, make_synth_table, prob_slider, replay_slider, start_ai_thread,
                   start_input_thread, start_output_thread, AITable, ChooserTable, SliderValue,
                   SynthTable, Preference, MelodyInfo, Database};
use std::mem;
use std::sync::{Arc, Mutex};
use enum_iterator::all;
use bare_metal_modulo::*;

fn main() -> anyhow::Result<()> {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Replayer",
        native_options,
        Box::new(|cc| Box::new(ReplayerApp::new(cc))),
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
    p_random_slider: Arc<Mutex<SliderValue<f64>>>,
    replay_delay_slider: Arc<Mutex<SliderValue<f64>>>,
    in_port: Option<MidiInputPort>,
    in_port_name: Option<String>,
    melody_pref: Preference,
    variation_pref: Preference,
    melody_info: VecTracker<MelodyInfo>,
    variation_info: VecTracker<MelodyInfo>,
    database: Arc<Mutex<Database>>
}

impl ReplayerApp {
    fn new(_cc: &eframe::CreationContext<'_>) -> Self {
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
        let p_random_slider = Arc::new(Mutex::new(prob_slider()));
        let replay_delay_slider = Arc::new(Mutex::new(replay_slider()));
        let database = Database::new();
        let mut melody_info = VecTracker::new(MelodyInfo::get_main_melodies(database.clone()));
        melody_info.go_to_end();
        let variation_info = VecTracker::new(if melody_info.is_empty() {vec![]} else {melody_info.get().unwrap().get_variations_of()});

        let app = ReplayerApp {
            midi_scenario: Arc::new(Mutex::new(MidiScenario::StartingUp)),
            midi_in: Arc::new(Mutex::new(None)),
            p_random_slider: p_random_slider.clone(),
            replay_delay_slider: replay_delay_slider.clone(),
            ai_table: ai_table.clone(),
            human_synth_table: human_synth_table.clone(),
            ai_synth_name,
            ai_synth_table: ai_synth_table.clone(),
            ai_name,
            human_synth_name,
            in_port: None,
            in_port_name: None,
            melody_pref: Preference::Neutral,
            variation_pref: Preference::Neutral,
            melody_info,
            variation_info,
            database
        };
        app.startup_thread();
        app
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
            Self::insert_slider(ui, self.replay_delay_slider.clone(), "Replay Delay");

            if !self.melody_info.is_empty() {
                self.sqlite_choice(ui);
            }
            self.update_melody_info();
        });
    }

    fn update_melody_info(&mut self) {
        let old_len = self.melody_info.len();
        self.melody_info.replace_vec(MelodyInfo::get_main_melodies(self.database.clone()));
        let changed = self.melody_info.len() > old_len;
        if changed {
            self.melody_info.go_to_end();
            self.melody_pref = self.melody_info.get().map_or(Preference::Neutral, |m| m.get_rating());
        }
        if !self.melody_info.is_empty() {
            let old_len = self.variation_info.len();
            self.variation_info.replace_vec(self.melody_info.get().unwrap().get_variations_of());
            let changed = changed || self.variation_info.len() > old_len;
            if changed {
                self.variation_info.go_to_end();
                self.variation_pref = self.variation_info.get().map_or(Preference::Neutral, |m| m.get_rating());
            }
        }
    }

    fn sqlite_choice(&mut self, ui: &mut Ui) {
        Self::melody_iterate(ui, &mut self.melody_info, &mut self.melody_pref);
        if !self.variation_info.is_empty() {
            Self::melody_iterate(ui, &mut self.variation_info, &mut self.variation_pref);
        }
    }

    fn melody_iterate(ui: &mut Ui, melodies: &mut VecTracker<MelodyInfo>, pref: &mut Preference) {
        ui.horizontal(|ui| {
            let stamp = {
                let info = melodies.get().unwrap();
                format!("{:?} {:?}", info.get_date(), info.get_time())
            };

            if !melodies.at_start() && ui.button("<").clicked() {
                melodies.go_left();
                *pref = melodies.get().map_or(Preference::Neutral, |m| m.get_rating());
            }
            ui.label(stamp.as_str());
            if !melodies.at_end() && ui.button(">").clicked() {
                melodies.go_right();
                *pref = melodies.get().map_or(Preference::Neutral, |m| m.get_rating());
            }
            Self::preference_buttons(ui, pref);
            melodies.get_mut().unwrap().update_rating(*pref);
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

    fn insert_slider(ui: &mut Ui, slider: Arc<Mutex<SliderValue<f64>>>, text: &str) {
        let mut slider = slider.lock().unwrap();
        let mut value = slider.get_current();
        ui.add(egui::Slider::new(&mut value, slider.make_range()).text(text));
        slider.set_current(value);
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
                self.start_now();
            }
        });
    }

    fn start_now(&mut self) {
        let in_port = self.in_port.as_ref().unwrap().clone();
        self.set_in_port_name(&in_port);
        let mut midi_in = None;
        {
            let mut self_midi_in = self.midi_in.lock().unwrap();
            mem::swap(&mut midi_in, &mut *self_midi_in);
        }
        let input2ai = Arc::new(SegQueue::new());
        let ai2output = Arc::new(SegQueue::new());

        start_output_thread(
            ai2output.clone(),
            self.human_synth_table.clone(),
            self.ai_synth_table.clone(),
        );
        start_ai_thread(
            self.ai_table.clone(),
            input2ai.clone(),
            ai2output,
            self.replay_delay_slider.clone(),
            self.p_random_slider.clone(),
            self.database.clone()
        );
        start_input_thread(
            input2ai,
            midi_in.unwrap(),
            self.in_port.as_ref().unwrap().clone(),
        );
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
                    self.start_now();
                }
                self.main_screen(ctx, frame)
            }
            MidiScenario::MultipleInputPorts { in_ports } => {
                self.pick_midi_screen(ctx, frame, &in_ports);
            }
        }
    }
}
