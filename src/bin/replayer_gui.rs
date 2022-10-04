use std::collections::BTreeMap;
use eframe::egui;
use midir::{MidiInput, MidiInputPort};
use musicserver1::{SliderValue, AIFunc, func_vec, get_midi_device, SynthFunc, wrap_func, MelodyMaker, Melody, sine_pulse, simple_tri, AITable, SynthTable, start_output, start_input, start_ai};
use std::sync::{Arc, Mutex};
use crossbeam_queue::SegQueue;

fn main() -> anyhow::Result<()> {
    let mut midi_in = MidiInput::new("midir reading input")?;
    let in_port = get_midi_device(&mut midi_in)?;
    let native_options = eframe::NativeOptions::default();
    eframe::run_native("Replayer", native_options, Box::new(|cc| Box::new(ReplayerApp::new(cc, in_port, midi_in))));
    Ok(())
}

struct ReplayerApp {
    in_port: MidiInputPort,
    ai_name: String,
    ai_table: Arc<Mutex<AITable>>,
    synth_name: String,
    synth_table: Arc<Mutex<SynthTable>>,
    p_random_slider: Arc<Mutex<SliderValue<f64>>>,
    replay_delay_slider: Arc<Mutex<SliderValue<f64>>>
}

impl ReplayerApp {
    fn new(cc: &eframe::CreationContext<'_>, in_port: MidiInputPort, midi_in: MidiInput) -> Self {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_visuals.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.
        let synth_funcs = func_vec![SynthFunc,
            ("Sine Pulse", sine_pulse),
            ("Simple Triangle", simple_tri)];
        let ai_funcs = func_vec![AIFunc,
            ("Bypass", |_,_,_| Melody::new()),
            ("Playback", |_, melody, _| melody.clone()),
            ("Greedy Choice", MelodyMaker::create_variation_1),
            ("Emphasis-Anchored Choice", MelodyMaker::create_variation_2),
            ("Consistent Figure Replacement", MelodyMaker::create_variation_4),
            ("Consistent Anchored Replacement", MelodyMaker::create_variation_3)];
        let ai_table = Arc::new(Mutex::new(AITable::from(&ai_funcs)));
        let synth_table = Arc::new(Mutex::new(SynthTable::from(&synth_funcs)));
        let ai_name = ai_funcs[0].name().to_string();
        let synth_name = synth_funcs[0].name().to_string();
        let p_random_slider = Arc::new(Mutex::new(SliderValue::new(1.0, 0.0, 1.0)));
        let replay_delay_slider = Arc::new(Mutex::new(SliderValue::new(1.5, 1.0, 5.0)));

        let app = ReplayerApp {
            in_port: in_port.clone(),
            p_random_slider: p_random_slider.clone(),
            replay_delay_slider: replay_delay_slider.clone(),
            ai_table: ai_table.clone(),
            synth_table: synth_table.clone(),
            ai_name,
            synth_name
        };

        let input2ai = Arc::new(SegQueue::new());
        let ai2output = Arc::new(SegQueue::new());

        start_output(ai2output.clone(), synth_table);
        start_ai(ai_table, input2ai.clone(), ai2output, replay_delay_slider, p_random_slider);
        start_input(input2ai, midi_in, in_port);
        app
    }
}

impl eframe::App for ReplayerApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Replayer");
            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    let mut ai_table = self.ai_table.lock().unwrap();
                    for func_name in ai_table.name_vec() {
                        ui.radio_value(&mut self.ai_name, func_name.clone(), func_name.clone());
                    }
                });
                ui.vertical(|ui| {
                    let mut synth_table = self.synth_table.lock().unwrap();
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
}