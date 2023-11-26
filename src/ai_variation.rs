use crate::analyzer::{Melody, MelodyMaker, MidiByte, Note};
use crate::database::{FromAiMsg, MelodyInfo, VariationStats};
use crate::runtime::{
    send_recorded_melody, ChooserTable, MelodyRunStatus, SliderValue, VariationControls,
    HUMAN_SPEAKER, VARIATION_SPEAKER,
};
use crate::{analyzer, arc_vec};
use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use eframe::emath::Numeric;
use midi_fundsp::io::SynthMsg;
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::Instant;

pub type AIFuncType = dyn Fn(&MelodyMaker, &Melody, f64) -> Melody + Send + Sync;
pub type AITable = ChooserTable<Arc<AIFuncType>>;
pub const NO_AI_NAME: &str = "Bypass";
pub const DEFAULT_AI_NAME: &str = "Figure Remapper";

pub fn make_ai_table() -> AITable {
    let ai_funcs: Vec<(String, Arc<AIFuncType>)> = arc_vec![
        (NO_AI_NAME, |_, _, _| Melody::new()),
        (DEFAULT_AI_NAME, MelodyMaker::create_remapped_variation),
        ("Motive Mapper", MelodyMaker::create_motive_variation)
    ];
    ChooserTable::from(&ai_funcs)
}

pub fn start_ai_thread(
    ai_table: Arc<Mutex<AITable>>,
    input2ai: Arc<SegQueue<SynthMsg>>,
    gui2ai: Arc<SegQueue<MelodyInfo>>,
    ai2output: Arc<SegQueue<SynthMsg>>,
    ai2dbase: Arc<SegQueue<FromAiMsg>>,
    variation_controls: VariationControls,
    replay_delay_slider: Arc<AtomicCell<SliderValue<f64>>>,
    melody_progress: Arc<AtomicCell<Option<f32>>>,
    melody_run_status: MelodyRunStatus,
) {
    std::thread::spawn(move || {
        let mut recorder = PlayerRecorder::new(
            input2ai,
            gui2ai,
            ai2output.clone(),
            replay_delay_slider.clone(),
        );
        let performer = Performer::new(variation_controls.clone(), ai_table);
        let min_melody_pitches = *analyzer::FIGURE_LENGTHS.iter().max().unwrap();

        loop {
            let incoming = recorder.record();
            if long_enough(
                incoming.melody(),
                min_melody_pitches,
                replay_delay_slider.load().current(),
            ) {
                let melody = incoming
                    .melody()
                    .without_brief_notes(variation_controls.shortest_note_slider.load().current());
                let variation = performer.create_variation(&melody);
                if long_enough(
                    &variation,
                    min_melody_pitches,
                    replay_delay_slider.load().current(),
                ) {
                    let stats = variation_controls.stats(performer.current_name());
                    ai2dbase.push(incoming.database_msg(&variation, stats));
                    melody_run_status.send_stop();
                    while melody_run_status.is_stopping() {}
                    send_recorded_melody(
                        &variation,
                        VARIATION_SPEAKER,
                        ai2output.clone(),
                        melody_progress.clone(),
                        melody_run_status.clone(),
                    );
                }
            }
        }
    });
}

fn _print_debug(melody: &Melody, label: &str) {
    println!("{label} ({} s): {melody:?}", melody.duration());
    melody.tuple_print();
}

fn long_enough(melody: &Melody, min_melody_pitches: usize, min_duration: f64) -> bool {
    melody.num_pitch_changes() >= min_melody_pitches && melody.duration() > min_duration
}

struct PlayerRecorder {
    input2ai: Arc<SegQueue<SynthMsg>>,
    gui2ai: Arc<SegQueue<MelodyInfo>>,
    ai2output: Arc<SegQueue<SynthMsg>>,
    replay_delay_slider: Arc<AtomicCell<SliderValue<f64>>>,
    waiting: Option<PendingNote>,
    player_melody: Melody,
}

impl PlayerRecorder {
    fn new(
        input2ai: Arc<SegQueue<SynthMsg>>,
        gui2ai: Arc<SegQueue<MelodyInfo>>,
        ai2output: Arc<SegQueue<SynthMsg>>,
        replay_delay_slider: Arc<AtomicCell<SliderValue<f64>>>,
    ) -> Self {
        PlayerRecorder {
            input2ai,
            gui2ai,
            ai2output,
            replay_delay_slider,
            waiting: None,
            player_melody: Melody::new(),
        }
    }

    fn record(&mut self) -> IncomingMelody {
        self.waiting = None;
        let mut player_finished = false;
        while !player_finished {
            if let Some(melody) = self.gui2ai.pop() {
                return IncomingMelody::Preexisting(melody);
            }
            if let Some(mut synth_msg) = self.input2ai.pop() {
                synth_msg.speaker = HUMAN_SPEAKER;
                self.handle_incoming(synth_msg);
            }

            if let Some(pending_note) = self.waiting {
                player_finished = self.check_if_finished(pending_note);
            }
        }
        let mut result = Melody::new();
        std::mem::swap(&mut result, &mut self.player_melody);
        result.synchronize_rests();
        IncomingMelody::New(result)
    }

    fn handle_incoming(&mut self, synth_msg: SynthMsg) {
        if let MidiMsg::ChannelVoice { channel: _, msg } = synth_msg.msg {
            match msg {
                ChannelVoiceMsg::NoteOff { note, velocity }
                | ChannelVoiceMsg::NoteOn { note, velocity } => {
                    if let Some(pending_note) = self.waiting {
                        self.player_melody.add(pending_note.into());
                    }
                    self.waiting = Some(PendingNote::new(note, velocity));
                }
                _ => {}
            }
        }
        self.ai2output.push(synth_msg);
    }

    fn check_if_finished(&mut self, pending_note: PendingNote) -> bool {
        let replay_delay = self.replay_delay_slider.load();
        if pending_note.is_rest() && pending_note.elapsed() > replay_delay.current() {
            self.player_melody.add(pending_note.into());
            true
        } else {
            false
        }
    }
}

struct Performer {
    maker: MelodyMaker,
    variation_controls: VariationControls,
    ai_table: Arc<Mutex<AITable>>,
}

impl Performer {
    fn new(variation_controls: VariationControls, ai_table: Arc<Mutex<AITable>>) -> Self {
        Performer {
            maker: MelodyMaker::new(),
            variation_controls,
            ai_table,
        }
    }

    fn from_slider<N: FromStr + Numeric>(slider: &Arc<AtomicCell<SliderValue<N>>>) -> N {
        slider.load().current()
    }

    fn current_name(&self) -> String {
        let ai_table = self.ai_table.lock().unwrap();
        ai_table.current_name().to_owned()
    }

    fn create_variation(&self, melody: &Melody) -> Melody {
        let p_random = Self::from_slider(&self.variation_controls.p_random_slider);
        let p_ornament = Self::from_slider(&self.variation_controls.p_ornament_slider);
        let whimsify = self.variation_controls.whimsify.load();
        let var_func = {
            let ai_table = self.ai_table.lock().unwrap();
            ai_table.current_choice()
        };
        let mut variation = var_func(&self.maker, &melody, p_random);
        if variation.len() > 0 && whimsify {
            variation = self.maker.whimsified_ending(&variation);
        }
        self.maker
            .ornamented(&melody.best_scale_for(), &variation, p_ornament)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct PendingNote {
    pitch: u8,
    timestamp: Instant,
    velocity: u8,
}

impl PendingNote {
    pub fn new(pitch: u8, velocity: u8) -> Self {
        PendingNote {
            pitch,
            timestamp: Instant::now(),
            velocity,
        }
    }

    pub fn pitch(&self) -> u8 {
        self.pitch
    }

    pub fn elapsed(&self) -> f64 {
        self.timestamp.elapsed().as_secs_f64()
    }

    pub fn is_rest(&self) -> bool {
        self.velocity == 0
    }

    pub fn instant_rest_from(&self) -> Note {
        Note::new(self.pitch as MidiByte, 0.0, 0)
    }
}

impl From<PendingNote> for Note {
    fn from(pending_note: PendingNote) -> Self {
        Note::new(
            pending_note.pitch as MidiByte,
            pending_note.elapsed(),
            pending_note.velocity as MidiByte,
        )
    }
}

#[derive(Clone)]
enum IncomingMelody {
    New(Melody),
    Preexisting(MelodyInfo),
}

impl IncomingMelody {
    fn melody(&self) -> &Melody {
        match self {
            IncomingMelody::New(melody) => melody,
            IncomingMelody::Preexisting(info) => info.melody(),
        }
    }

    fn database_msg(&self, variation: &Melody, stats: VariationStats) -> FromAiMsg {
        match self {
            IncomingMelody::New(melody) => FromAiMsg::MelodyVariation {
                melody: melody.clone(),
                variation: variation.clone(),
                stats,
            },
            IncomingMelody::Preexisting(info) => FromAiMsg::AlternateVariation {
                melody_id: info.row_id(),
                variation: variation.clone(),
                stats,
            },
        }
    }
}
