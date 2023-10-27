use std::sync::{Arc, Mutex};

use eframe::{
    egui::FontDefinitions,
    epaint::{Pos2, Vec2},
};
use midi_fundsp::SynthFunc;
use midir::{MidiInput, MidiInputPort};
use musicserver1::{
    load_font,
    midi::MidiScenario,
    runtime::{make_synth_table, TableInfo},
};

fn main() {
    let mut native_options = eframe::NativeOptions::default();
    native_options.initial_window_size = Some(Vec2 { x: 800.0, y: 600.0 });
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
    in_port: Option<MidiInputPort>,
    in_port_name: Option<String>,
}

impl SynthApp {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut fonts = FontDefinitions::default();
        load_font!(fonts, "../../bravura/BravuraText.otf");
        cc.egui_ctx.set_fonts(fonts);

        let synth = TableInfo::new(make_synth_table());
        SynthApp {
            midi_scenario: Arc::new(Mutex::new(MidiScenario::StartingUp)),
            midi_in: Arc::new(Mutex::new(None)),
            synth,
            in_port: None,
            in_port_name: None,
        }
    }
}

impl eframe::App for SynthApp {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        todo!()
    }
}
