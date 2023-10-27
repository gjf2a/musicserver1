use std::sync::{Arc, Mutex};

use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use eframe::{
    egui::{self, FontDefinitions, Visuals},
    epaint::{Pos2, Vec2},
};
use midi_fundsp::{
    io::{start_input_thread, start_output_thread, Speaker, SynthMsg},
    SynthFunc,
};
use midir::{MidiInput, MidiInputPort, MidiInputPorts};
use musicserver1::{
    load_font,
    midi::MidiScenario,
    runtime::{make_synth_table, TableInfo},
};

const NUM_OUTPUT_CHANNELS: usize = 10;

fn main() {
    let mut native_options = eframe::NativeOptions::default();
    native_options.initial_window_size = Some(Vec2 { x: 400.0, y: 400.0 });
    native_options.initial_window_pos = Some(Pos2 { x: 50.0, y: 25.0 });
    eframe::run_native(
        "Synth",
        native_options,
        Box::new(|cc| Box::new(SynthApp::new(cc))),
    )
    .unwrap();
}

struct SynthApp {
    midi_scenario: Arc<Mutex<MidiScenario>>,
    midi_in: Arc<Mutex<Option<MidiInput>>>,
    synth: TableInfo<SynthFunc>,
    input2output: Arc<SegQueue<SynthMsg>>,
    in_port: Option<MidiInputPort>,
    in_port_name: Option<String>,
    quit_threads: Arc<AtomicCell<bool>>,
}

impl eframe::App for SynthApp {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
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
                    self.start_input();
                }
                self.main_screen(ctx, frame)
            }
            MidiScenario::MultipleInputPorts { in_ports } => {
                self.pick_midi_screen(ctx, frame, &in_ports);
            }
        }
    }
}

impl SynthApp {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut fonts = FontDefinitions::default();
        load_font!(fonts, "../../bravura/BravuraText.otf");
        cc.egui_ctx.set_fonts(fonts);

        let synth = TableInfo::new(make_synth_table());
        let mut app = SynthApp {
            midi_scenario: Arc::new(Mutex::new(MidiScenario::StartingUp)),
            midi_in: Arc::new(Mutex::new(None)),
            synth,
            in_port: None,
            in_port_name: None,
            input2output: Arc::new(SegQueue::new()),
            quit_threads: Arc::new(AtomicCell::new(false)),
        };
        app.startup();
        app
    }

    fn startup_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Looking for MIDI devices...\nMove mouse to continue");
        });
    }

    fn startup(&mut self) {
        start_output_thread::<NUM_OUTPUT_CHANNELS>(
            self.input2output.clone(),
            Arc::new(Mutex::new(self.synth.choice_vec())),
            self.quit_threads.clone(),
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
            ui.heading(format!("No MIDI devices connected\n{message}"));
        });
    }

    fn main_screen(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            let heading = format!(
                "MIDI FunDSP GUI using ({})",
                self.in_port_name.as_ref().unwrap()
            );
            ui.heading(heading);
            let current_synth = self.synth.current_name();
            self.synth.radio_choice(ui, "Synth choices");
            if self.synth.current_name() != current_synth {
                self.input2output.push(SynthMsg::program_change(
                    self.synth.current_index() as u8,
                    Speaker::Both,
                ));
            }
        });
    }

    fn start_input(&mut self) {
        let in_port = self.in_port.as_ref().unwrap().clone();
        self.set_in_port_name(&in_port);
        let midi_in = {
            let mut self_midi_in = self.midi_in.lock().unwrap();
            self_midi_in.take()
        };

        start_input_thread(
            self.input2output.clone(),
            midi_in.unwrap(),
            self.in_port.as_ref().unwrap().clone(),
            self.quit_threads.clone(),
        );
    }

    fn set_in_port_name(&mut self, in_port: &MidiInputPort) {
        let midi_in = self.midi_in.lock().unwrap();
        self.in_port_name = midi_in.as_ref().and_then(|m| m.port_name(in_port).ok());
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
                self.start_input();
            }
        });
    }
}
