use std::str::FromStr;
use crate::{arc_vec, ChooserTable, Melody, MelodyMaker, PendingNote, SliderValue, SynthChoice, FromAiMsg};
use crossbeam_queue::SegQueue;
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use eframe::emath::Numeric;

pub type AIFuncType = dyn Fn(&mut MelodyMaker, &Melody, f64) -> Melody + Send + Sync;
pub type AITable = ChooserTable<Arc<AIFuncType>>;

pub fn make_ai_table() -> AITable {
    let ai_funcs: Vec<(&str, Arc<AIFuncType>)> = arc_vec![
        ("Bypass", |_, _, _| Melody::new()),
        ("Emphasis-Anchored Choice", MelodyMaker::create_emphasis_variation),
        ("Consistent Figure Replacement", MelodyMaker::create_figure_mapped_variation)
    ];
    ChooserTable::from(&ai_funcs)
}

pub fn start_ai_thread(
    ai_table: Arc<Mutex<AITable>>,
    input2ai: Arc<SegQueue<MidiMsg>>,
    ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>,
    ai2dbase: Arc<SegQueue<FromAiMsg>>,
    replay_delay_slider: Arc<Mutex<SliderValue<f64>>>,
    ornament_gap_slider: Arc<Mutex<SliderValue<i64>>>,
    p_random_slider: Arc<Mutex<SliderValue<f64>>>,
) {
    std::thread::spawn(move || {
        let mut recorder = PlayerRecorder::new(
            input2ai.clone(),
            ai2output.clone(),
            replay_delay_slider.clone(),
        );
        let mut performer =
            Performer::new(p_random_slider.clone(), ornament_gap_slider.clone(), ai_table.clone(), ai2output.clone());
        loop {
            let melody = recorder.record();
            let variation = performer.create_variation(&melody);
            if variation.len() > 0 {
                ai2dbase.push(FromAiMsg {melody, variation: variation.clone()});
                performer.send_variation(&variation);
            }
        }
    });
}

struct PlayerRecorder {
    input2ai: Arc<SegQueue<MidiMsg>>,
    ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>,
    replay_delay_slider: Arc<Mutex<SliderValue<f64>>>,
    waiting: Option<PendingNote>,
    player_melody: Melody,
}

impl PlayerRecorder {
    fn new(
        input2ai: Arc<SegQueue<MidiMsg>>,
        ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>,
        replay_delay_slider: Arc<Mutex<SliderValue<f64>>>,
    ) -> Self {
        PlayerRecorder {
            input2ai,
            ai2output,
            replay_delay_slider,
            waiting: None,
            player_melody: Melody::new(),
        }
    }

    fn record(&mut self) -> Melody {
        self.waiting = None;
        let mut player_finished = false;
        while !player_finished {
            if let Some(msg) = self.input2ai.pop() {
                self.handle_incoming(msg);
            }

            if let Some(pending_note) = self.waiting {
                player_finished = self.check_if_finished(pending_note);
            }
        }
        let mut result = Melody::new();
        std::mem::swap(&mut result, &mut self.player_melody);
        result.synchronize_rests();
        result
    }

    fn handle_incoming(&mut self, msg: MidiMsg) {
        if let MidiMsg::ChannelVoice { channel: _, msg } = msg {
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
        self.ai2output.push((SynthChoice::Human, msg));
    }

    fn check_if_finished(&mut self, pending_note: PendingNote) -> bool {
        let replay_delay = self.replay_delay_slider.lock().unwrap();
        if pending_note.is_rest() && pending_note.elapsed() > replay_delay.get_current() {
            self.player_melody.add(pending_note.into());
            true
        } else {
            false
        }
    }
}

struct Performer {
    maker: MelodyMaker,
    p_random_slider: Arc<Mutex<SliderValue<f64>>>,
    ornament_gap_slider: Arc<Mutex<SliderValue<i64>>>,
    ai_table: Arc<Mutex<AITable>>,
    ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>
}

impl Performer {
    fn new(
        p_random_slider: Arc<Mutex<SliderValue<f64>>>,
        ornament_gap_slider: Arc<Mutex<SliderValue<i64>>>,
        ai_table: Arc<Mutex<AITable>>,
        ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>,
    ) -> Self {
        Performer {
            maker: MelodyMaker::new(),
            p_random_slider,
            ornament_gap_slider,
            ai_table,
            ai2output
        }
    }

    fn from_slider<N:FromStr + Numeric>(slider: &Arc<Mutex<SliderValue<N>>>) -> N {
        let slider = slider.lock().unwrap();
        slider.get_current()
    }

    fn create_variation(&mut self, melody: &Melody) -> Melody {
        let p_random = Self::from_slider(&self.p_random_slider);
        let ornament_gap = Self::from_slider(&self.ornament_gap_slider);
        let var_func = {
            let ai_table = self.ai_table.lock().unwrap();
            ai_table.current_choice()
        };
        let variation = var_func(&mut self.maker, &melody, p_random);
        self.maker.ornamented(&variation, ornament_gap)
    }

    fn send_variation(&self, variation: &Melody) {
        for note in variation.iter() {
            let (midi, duration) = note.to_midi();
            self.ai2output.push((SynthChoice::Ai, midi));
            std::thread::sleep(Duration::from_secs_f64(duration));
        }
    }
}
