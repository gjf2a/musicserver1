use crate::egui::Ui;
use crossbeam_queue::SegQueue;
use eframe::egui;
use midir::{Ignore, MidiInput, MidiInputPort, MidiInputPorts};
use musicserver1::{make_ai_table, make_synth_table, prob_slider, replay_slider, start_ai_thread, start_input_thread, start_output_thread, AITable, ChooserTable, SliderValue, SynthTable, Preference};
use std::mem;
use std::sync::{Arc, Mutex};
use enum_iterator::all;

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
    variation_pref: Preference
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
            variation_pref: Preference::Neutral
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

            self.sqlite_choice(ui);
        });
    }

    fn sqlite_choice(&mut self, ui: &mut Ui) {
        ui.horizontal(|ui| {
            if ui.button("<").clicked() {

            }
            ui.label("Melody timestamp here");
            if ui.button(">").clicked() {

            }
            Self::preference_buttons(ui, &mut self.melody_pref);
        });


        ui.horizontal(|ui| {
            if ui.button("<").clicked() {

            }
            ui.label("Variation timestamp here");
            if ui.button(">").clicked() {

            }
            Self::preference_buttons(ui, &mut self.variation_pref);
        });
    }

    fn preference_buttons(ui: &mut Ui, pref: &mut Preference) {
        ui.horizontal(|ui| {
            for preference in all::<Preference>() {
                ui.radio_value(pref, preference, preference.to_string());
            }
        });
    }

    fn radio_choice<T: Clone>(
        ui: &mut Ui,
        header: &str,
        table: Arc<Mutex<ChooserTable<T>>>,
        tag: &mut String,
    ) {
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
            true
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
