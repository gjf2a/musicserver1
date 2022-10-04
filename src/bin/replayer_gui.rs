use std::mem;
use eframe::egui;
use midir::{Ignore, MidiInput, MidiInputPort, MidiInputPorts};
use musicserver1::{SliderValue, AITable, SynthTable, start_output_thread, start_input_thread, start_ai_thread, make_ai_table, make_synth_table, prob_slider, replay_slider};
use std::sync::{Arc, Mutex};
use crossbeam_queue::SegQueue;

fn main() -> anyhow::Result<()> {
    let mut midi_in = MidiInput::new("midir reading input");
    let scenario = match midi_in {
        Ok(ref mut midi_in) => {
            midi_in.ignore(Ignore::None);
            let in_ports = midi_in.ports();
            match in_ports.len() {
                0 => MidiScenario::NoInputPorts("No MIDI devices found".to_string()),
                1 => MidiScenario::InputPortSelected {in_port: in_ports[0].clone()},
                _ => MidiScenario::MultipleInputPorts {in_ports: in_ports.clone()}
            }
        }
        Err(e) => {
            MidiScenario::NoInputPorts(e.to_string())
        }
    };
    let native_options = eframe::NativeOptions::default();
    eframe::run_native("Replayer", native_options, Box::new(|cc| Box::new(ReplayerApp::new(cc, scenario, midi_in.ok()))));
    Ok(())
}

#[derive(Clone)]
enum MidiScenario {
    NoInputPorts(String),
    InputPortSelected {in_port: MidiInputPort},
    MultipleInputPorts {in_ports: MidiInputPorts}
}

struct ReplayerApp {
    midi_scenario: MidiScenario,
    midi_in: Option<MidiInput>,
    ai_name: String,
    ai_table: Arc<Mutex<AITable>>,
    synth_name: String,
    synth_table: Arc<Mutex<SynthTable>>,
    p_random_slider: Arc<Mutex<SliderValue<f64>>>,
    replay_delay_slider: Arc<Mutex<SliderValue<f64>>>,
    in_port: Option<MidiInputPort>,
    in_port_name: Option<String>
}

impl ReplayerApp {
    fn new(_cc: &eframe::CreationContext<'_>, midi_state: MidiScenario, midi_in: Option<MidiInput>) -> Self {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_visuals.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.

        let ai_table = make_ai_table();
        let synth_table = make_synth_table();
        let ai_name = ai_table.current_name().to_string();
        let synth_name = synth_table.current_name().to_string();
        let ai_table = Arc::new(Mutex::new(ai_table));
        let synth_table = Arc::new(Mutex::new(synth_table));
        let p_random_slider = Arc::new(Mutex::new(prob_slider()));
        let replay_delay_slider = Arc::new(Mutex::new(replay_slider()));

        ReplayerApp {
            midi_scenario: midi_state,
            midi_in,
            p_random_slider: p_random_slider.clone(),
            replay_delay_slider: replay_delay_slider.clone(),
            ai_table: ai_table.clone(),
            synth_table: synth_table.clone(),
            ai_name,
            synth_name,
            in_port: None,
            in_port_name: None
        }
    }

    fn main_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Replayer");
            ui.label(format!("Using {}", self.in_port_name.as_ref().unwrap()));
            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    let ai_table = self.ai_table.lock().unwrap();
                    for func_name in ai_table.name_vec() {
                        ui.radio_value(&mut self.ai_name, func_name.clone(), func_name.clone());
                    }
                });
                ui.vertical(|ui| {
                    let synth_table = self.synth_table.lock().unwrap();
                    for func_name in synth_table.name_vec() {
                        ui.radio_value(&mut self.synth_name, func_name.clone(), func_name.clone());
                    }
                });
            });
            let mut ai_table = self.ai_table.lock().unwrap();
            ai_table.choose(self.ai_name.as_str());
            let mut synth_table = self.synth_table.lock().unwrap();
            synth_table.choose(self.synth_name.as_str());

            let mut p_random_slider = self.p_random_slider.lock().unwrap();
            let mut replay_delay_slider = self.replay_delay_slider.lock().unwrap();
            let mut p_random = p_random_slider.get_current();
            let mut replay_delay = replay_delay_slider.get_current();

            ui.add(egui::Slider::new(&mut p_random, p_random_slider.make_range()).text("Probability of Randomization"));
            ui.add(egui::Slider::new(&mut replay_delay, replay_delay_slider.make_range()).text("Replay Delay"));
            p_random_slider.set_current(p_random);
            replay_delay_slider.set_current(replay_delay);
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
                    ui.radio_value(&mut self.in_port, Some(in_port.clone()), self.in_port_name.as_ref().unwrap().clone());
                }
            });
            if ui.button("Start Playing").clicked() {
                self.midi_scenario = MidiScenario::InputPortSelected {in_port: self.in_port.clone().unwrap()};
                self.start_now();
            }
        });
    }

    fn start_now(&mut self) {
        let in_port = self.in_port.as_ref().unwrap().clone();
        self.set_in_port_name(&in_port);
        let mut midi_in = None;
        mem::swap(&mut midi_in, &mut self.midi_in);
        let input2ai = Arc::new(SegQueue::new());
        let ai2output = Arc::new(SegQueue::new());

        start_output_thread(ai2output.clone(), self.synth_table.clone());
        start_ai_thread(self.ai_table.clone(), input2ai.clone(), ai2output, self.replay_delay_slider.clone(), self.p_random_slider.clone());
        start_input_thread(input2ai, midi_in.unwrap(), self.in_port.as_ref().unwrap().clone());
    }

    fn set_in_port_name(&mut self, in_port: &MidiInputPort) {
        self.in_port_name = self.midi_in.as_ref().and_then(|m| m.port_name(in_port).ok());
    }
}

impl eframe::App for ReplayerApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        let scenario = self.midi_scenario.clone();
        match scenario {
            MidiScenario::NoInputPorts(msg) => {
                self.no_midi_screen(ctx, frame, msg.as_str());
            }
            MidiScenario::InputPortSelected {in_port} => {
                if self.in_port.is_none() {
                    self.in_port = Some(in_port);
                    self.start_now();
                }
                self.main_screen(ctx, frame)
            }
            MidiScenario::MultipleInputPorts {in_ports} => {
                self.pick_midi_screen(ctx, frame, &in_ports);
            }
        }
    }
}