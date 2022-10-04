use std::collections::HashMap;
use eframe::egui;
use midir::{MidiInput, MidiInputPort};
use musicserver1::{AIFunc, func_map, get_midi_device, SynthFunc, wrap_func, MelodyMaker, Melody, sine_pulse, simple_tri};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let mut midi_in = MidiInput::new("midir reading input")?;
    let in_port = get_midi_device(&mut midi_in)?;
    let native_options = eframe::NativeOptions::default();
    eframe::run_native("Replayer", native_options, Box::new(|cc| Box::new(ReplayerApp::new(cc, in_port))));
    Ok(())
}

struct ReplayerApp {
    in_port: MidiInputPort,
    ai: String,
    ai_funcs: HashMap<String,AIFunc>,
    synth: String,
    synth_funcs: HashMap<String,SynthFunc>,
    p_random: f64
}

impl ReplayerApp {
    fn new(cc: &eframe::CreationContext<'_>, in_port: MidiInputPort) -> Self {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_visuals.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.
        let synth_funcs = func_map![SynthFunc,
            ("Sine Pulse", sine_pulse),
            ("Simple Triangle", simple_tri)];
        let ai_funcs = func_map![AIFunc,
            ("Bypass", |_,_,_| Melody::new()),
            ("Playback", |_, melody, _| melody.clone()),
            ("Greedy Choice", MelodyMaker::create_variation_1),
            ("Emphasis-Anchored Choice", MelodyMaker::create_variation_2),
            ("Consistent Figure Replacement", MelodyMaker::create_variation_4),
            ("Consistent Anchored Replacement", MelodyMaker::create_variation_3)];
        ReplayerApp {in_port, p_random: 1.0, ai: ai_funcs.keys().next().unwrap().clone(), ai_funcs, synth: synth_funcs.keys().next().unwrap().clone(), synth_funcs}
    }
}

impl eframe::App for ReplayerApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("My egui Application");
            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    for func_name in self.ai_funcs.keys() {
                        ui.radio_value(&mut self.ai, func_name.clone(), func_name.clone());
                    }
                });
                ui.vertical(|ui| {
                    for func_name in self.synth_funcs.keys() {
                        ui.radio_value(&mut self.synth, func_name.clone(), func_name.clone());
                    }
                });
            });
            ui.add(egui::Slider::new(&mut self.p_random, 0.0..=1.0).text("Probability of randomization"));
        });
    }
}