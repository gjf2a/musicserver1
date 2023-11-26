use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use eframe::egui::{self, Key, TextEdit};
use eframe::egui::{Color32, FontDefinitions, Pos2, Ui, Vec2, Visuals};
use eframe::emath::Numeric;
use enum_iterator::all;
use midi_fundsp::io::{start_input_thread, start_output_thread, Speaker, SynthMsg};
use midi_fundsp::SynthFunc;
use midir::{MidiInput, MidiInputPort, MidiInputPorts};
use musicserver1::ai_variation::{
    make_ai_table, start_ai_thread, AIFuncType, DEFAULT_AI_NAME, NO_AI_NAME,
};
use musicserver1::analyzer::Melody;
use musicserver1::database::{
    start_database_thread, Database, DatabaseGuiUpdate, FromAiMsg, GuiDatabaseUpdate, MelodyInfo,
    Preference, VariationStats,
};
use musicserver1::load_font;
use musicserver1::melody_renderer::MelodyRenderer;
use musicserver1::midi::MidiScenario;
use musicserver1::runtime::{
    make_synth_table, replay_slider, send_recorded_melody, send_two_melodies, MelodyRunStatus,
    SliderValue, SynthChoice, TableInfo, VariationControls, HUMAN_SPEAKER, VARIATION_SPEAKER,
};
use musicserver1::vec_tracker::VecTracker;
use std::fmt::Display;
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

const NUM_OUTPUT_CHANNELS: usize = 10; // More than this, and it has occasional noise-clipping problems.
const MAIN_MELODY_SCALING: f32 = 0.8;

fn main() {
    let native_options = eframe::NativeOptions {
           viewport: egui::ViewportBuilder::default()
               .with_inner_size(Vec2 { x: 800.0, y: 600.0 })
               .with_position(Pos2 { x: 50.0, y: 25.0 })
               .with_drag_and_drop(true),
             ..Default::default()
         };
    eframe::run_native(
        "Replayer",
        native_options,
        Box::new(|cc| Box::new(ReplayerApp::new(cc).unwrap())),
    )
    .unwrap();
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
            self.show_synths(ui);
            self.ai_algorithm.radio_choice(ui, "Variation Algorithm");
        });
    }

    fn show_synths(&mut self, ui: &mut Ui) {
        let human_name = self.human_synth.current_name();
        let ai_name = self.ai_synth.current_name();
        if self.show_synth_choices {
            self.human_synth.radio_choice(ui, "Human Synthesizer");
            self.ai_synth.radio_choice(ui, "Variation Synthesizer");
        } else {
            ui.vertical(|ui| {
                ui.label(format!("Human synth: {}", self.human_synth.current_name()));
                ui.label(format!("AI synth: {}", self.ai_synth.current_name()));
            });
        }
        self.update_synths(human_name.as_str(), ai_name.as_str());
    }

    fn update_synths(&self, human_name: &str, ai_name: &str) {
        if human_name != self.human_synth.current_name() {
            let msg =
                SynthMsg::program_change(self.human_synth.current_index() as u8, HUMAN_SPEAKER);
            self.ai2output.push(msg);
        }
        if ai_name != self.ai_synth.current_name() {
            let msg =
                SynthMsg::program_change(self.ai_synth.current_index() as u8, VARIATION_SPEAKER);
            self.ai2output.push(msg);
        }
    }

    fn variation_parameter_settings(&mut self, ui: &mut Ui) {
        ui.vertical(|ui| {
            ui.label("Variation Algorithm Controls");
            /*let p_random_slider = self.variation_controls.p_random_slider.clone();
            Self::insert_slider(ui, p_random_slider, "Probability of Randomization");
            let p_ornament_slider = self.variation_controls.p_ornament_slider.clone();
            Self::insert_slider(ui, p_ornament_slider, "Probability of Inserting Ornament");*/
            let replay_delay_slider = self.replay_delay_slider.clone();
            Self::insert_slider(ui, replay_delay_slider, "Replay Delay (seconds)");
            /*let shortest_note_slider = self.variation_controls.shortest_note_slider.clone();
            Self::insert_slider(ui, shortest_note_slider, "Shortest Playable Note (seconds)");
            let mut whimsify = self.variation_controls.whimsify.load();
            ui.checkbox(&mut whimsify, "Whimsify Suffix");
            self.variation_controls.whimsify.store(whimsify);*/
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
            self.ai_algorithm
                .update_choice(stats.algorithm_name.as_str());
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
        let mut melodies = vec![(melody_info.melody().clone(), Color32::BLACK)];
        if self.show_variation {
            melodies.push((variation_info.melody().clone(), Color32::RED));
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
        if self.ai_algorithm.current_name() == NO_AI_NAME {
            self.ai_algorithm.update_choice(DEFAULT_AI_NAME);
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
            Arc::new(Mutex::new(self.human_synth.choice_vec())),
            self.quit_threads.clone(),
        );
        start_ai_thread(
            self.ai_algorithm.table_ref(),
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
