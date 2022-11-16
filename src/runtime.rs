use read_input::prelude::input;
use read_input::InputBuild;
use std::collections::btree_map::BTreeMap;
use std::ops::RangeInclusive;
use std::sync::Arc;
use std::time::Duration;
use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use crate::analyzer::Melody;
use crate::synth_output::SynthOutputMsg;

pub const SHOW_MIDI_MSG: bool = false;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum SynthChoice {
    Human,
    Ai,
}

pub struct ChooserTable<T: Clone> {
    name2choice: BTreeMap<String, T>,
    names: Vec<String>,
    current_name: String,
}

impl<T: Clone> ChooserTable<T> {
    pub fn from(choices: &Vec<(&str, T)>) -> Self {
        let current_name = choices.iter().next().unwrap().0.to_string();
        let mut name2choice = BTreeMap::new();
        for (name, choice) in choices.iter() {
            name2choice.insert(name.to_string(), choice.clone());
        }
        let names: Vec<String> = choices.iter().map(|c| c.0.to_string()).collect();
        ChooserTable {
            name2choice,
            names,
            current_name,
        }
    }

    pub fn choose(&mut self, choice: &str) {
        assert!(self.name2choice.contains_key(choice));
        self.current_name = choice.to_owned();
    }

    pub fn current_name(&self) -> &str {
        self.current_name.as_str()
    }

    pub fn current_choice(&self) -> T {
        self.name2choice
            .get(self.current_name.as_str())
            .unwrap()
            .clone()
    }

    pub fn name_vec(&self) -> Vec<String> {
        self.names.clone()
    }

    pub fn console_pick(&mut self) {
        self.current_name = user_pick_element(self.names.iter().cloned(), |s| s.clone())
    }
}

#[macro_export]
macro_rules! arc_vec {
    ($( ($s:expr, $f:expr)),* ) => {vec![$(($s, Arc::new($f)),)*]}
}

#[derive(Clone)]
pub struct VariationControlSliders {
    pub p_random_slider: Arc<AtomicCell<SliderValue<f64>>>,
    pub p_ornament_slider: Arc<AtomicCell<SliderValue<f64>>>,
    pub whimsification_slider: Arc<AtomicCell<SliderValue<f64>>>,
    pub shortest_note_slider: Arc<AtomicCell<SliderValue<f64>>>,
}

impl VariationControlSliders {
    pub fn new() -> Self {
        Self {
            p_random_slider: Arc::new(AtomicCell::new(prob_slider(0.8))),
            p_ornament_slider: Arc::new(AtomicCell::new(prob_slider(0.2))),
            whimsification_slider: Arc::new(AtomicCell::new(prob_slider(0.0))),
            shortest_note_slider: Arc::new(AtomicCell::new(SliderValue::new(0.1, 0.0, 0.2)))
        }
    }
}

#[derive(Copy, Clone)]
pub struct SliderValue<T: Copy + Clone> {
    current: T,
    lo: T,
    hi: T,
}

impl<T: Copy + Clone + std::str::FromStr + PartialOrd + 'static> SliderValue<T> {
    pub fn new(current: T, min: T, max: T) -> Self {
        SliderValue {
            current,
            lo: min,
            hi: max,
        }
    }

    pub fn make_range(&self) -> RangeInclusive<T> {
        self.lo..=self.hi
    }

    pub fn slid_to(&self, new_current: T) -> Self {
        SliderValue {current: new_current, lo: self.lo, hi: self.hi}
    }

    pub fn set_current(&mut self, new_current: T) {
        self.current = new_current;
    }

    pub fn current(&self) -> T {
        self.current
    }

    pub fn console_pick(&mut self, prompt: &str) {
        self.current = input().msg(prompt).inside(self.make_range()).get();
    }
}

pub fn replay_slider() -> SliderValue<f64> {
    SliderValue::new(1.5, 1.0, 5.0)
}

pub fn prob_slider(start_prob: f64) -> SliderValue<f64> {
    SliderValue::new(start_prob, 0.0, 1.0)
}

pub fn user_pick_element<T: Clone, S: Fn(&T) -> String>(
    choices: impl Iterator<Item = T>,
    show: S,
) -> T {
    let choices = choices.collect::<Vec<_>>();
    for (i, item) in choices.iter().enumerate() {
        println!("{}) {}", i + 1, show(item));
    }
    let choice: usize = input()
        .msg("Enter choice: ")
        .inside(1..=choices.len())
        .get();
    choices[choice - 1].clone()
}

pub fn send_recorded_melody(melody: &Melody, synth: SynthChoice, ai2output: Arc<SegQueue<SynthOutputMsg>>) {
    for note in melody.iter() {
        let (midi, duration) = note.to_midi();
        ai2output.push(SynthOutputMsg {synth, midi});
        std::thread::sleep(Duration::from_secs_f64(duration));
    }
}