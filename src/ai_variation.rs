use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use crossbeam_queue::SegQueue;
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use crate::{user_pick_element, func_vec, make_chooser_table, Melody, MelodyMaker, PendingNote, SliderValue};

#[derive(Clone)]
pub struct AIFunc {
    name: String,
    func: Arc<dyn Fn(&mut MelodyMaker,&Melody,f64)->Melody + Send + Sync>
}

impl AIFunc {
    pub fn new(name: &str, func: Arc<dyn Fn(&mut MelodyMaker,&Melody,f64)->Melody + Send + Sync>) -> Self {
        AIFunc {name: name.to_owned(), func}
    }
    pub fn name(&self) -> &str {self.name.as_str()}
    pub fn func(&self) -> Arc<dyn Fn(&mut MelodyMaker,&Melody,f64)->Melody + Send + Sync> {
        self.func.clone()
    }
}

make_chooser_table!{AITable, AIFunc}

pub fn make_ai_table() -> AITable {
    let ai_funcs = func_vec![AIFunc,
            ("Bypass", |_,_,_| Melody::new()),
            ("Playback", |_, melody, _| melody.clone()),
            ("Greedy Choice", MelodyMaker::create_variation_1),
            ("Emphasis-Anchored Choice", MelodyMaker::create_variation_2),
            ("Consistent Figure Replacement", MelodyMaker::create_variation_4),
            ("Consistent Anchored Replacement", MelodyMaker::create_variation_3)];
    AITable::from(&ai_funcs)
}


pub fn start_ai_thread(ai_table: Arc<Mutex<AITable>>, input2ai: Arc<SegQueue<MidiMsg>>, ai2output: Arc<SegQueue<MidiMsg>>,
                       replay_delay_slider: Arc<Mutex<SliderValue<f64>>>,
                       p_random_slider: Arc<Mutex<SliderValue<f64>>>) {
    std::thread::spawn(move || {
        let mut recorder = PlayerRecorder::new(input2ai.clone(), ai2output.clone(), replay_delay_slider.clone());
        let mut performer = Performer::new(p_random_slider.clone(), ai_table.clone(), ai2output.clone());
        loop {
            let player_melody = recorder.record();
            performer.perform_variation(&player_melody);
        }
    });
}

struct PlayerRecorder {
    input2ai: Arc<SegQueue<MidiMsg>>,
    ai2output: Arc<SegQueue<MidiMsg>>,
    replay_delay_slider: Arc<Mutex<SliderValue<f64>>>,
    waiting: Option<PendingNote>,
    player_melody: Melody
}

impl PlayerRecorder {
    fn new(input2ai: Arc<SegQueue<MidiMsg>>, ai2output: Arc<SegQueue<MidiMsg>>, replay_delay_slider: Arc<Mutex<SliderValue<f64>>>) -> Self {
        PlayerRecorder {
            input2ai, ai2output, replay_delay_slider, waiting: None, player_melody: Melody::new()
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
                ChannelVoiceMsg::NoteOff { note, velocity } | ChannelVoiceMsg::NoteOn { note, velocity } => {
                    if let Some(pending_note) = self.waiting {
                        self.player_melody.add(pending_note.into());
                    }
                    self.waiting = Some(PendingNote::new(note, velocity));
                }
                _ => {}
            }
        }
        self.ai2output.push(msg);
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
    ai_table: Arc<Mutex<AITable>>,
    ai2output: Arc<SegQueue<MidiMsg>>
}

impl Performer {
    fn new(p_random_slider: Arc<Mutex<SliderValue<f64>>>, ai_table: Arc<Mutex<AITable>>, ai2output: Arc<SegQueue<MidiMsg>>) -> Self {
        Performer {maker: MelodyMaker::new(), p_random_slider, ai_table, ai2output}
    }

    fn perform_variation(&mut self, player_melody: &Melody) {
        let variation = {
            let p_random = self.p_random_slider.lock().unwrap();
            let ai_table = self.ai_table.lock().unwrap();
            (ai_table.current_func().func)(&mut self.maker, &player_melody, p_random.get_current())
        };

        for note in variation.iter() {
            let (midi, duration) = note.to_midi();
            self.ai2output.push(midi);
            std::thread::sleep(Duration::from_secs_f64(duration));
        }
    }
}