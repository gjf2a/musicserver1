use crate::analyzer::Melody;
use crate::database::VariationStats;
use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use midi_fundsp::io::{Speaker, SynthMsg};
use midi_fundsp::sounds::favorites;
use midi_fundsp::SynthFunc;
use midi_msg::MidiMsg;
use read_input::prelude::input;
use read_input::InputBuild;
use std::collections::btree_map::BTreeMap;
use std::collections::VecDeque;
use std::ops::RangeInclusive;
use std::sync::{Arc, Mutex};
use std::time::Instant;

pub const SHOW_MIDI_MSG: bool = false;

pub const HUMAN_SPEAKER: Speaker = Speaker::Left;
pub const VARIATION_SPEAKER: Speaker = Speaker::Right;

pub struct TableInfo<T: Clone> {
    table: Arc<Mutex<ChooserTable<T>>>,
}

impl<T: Clone> TableInfo<T> {
    pub fn new(table: ChooserTable<T>) -> Self {
        let table = Arc::new(Mutex::new(table));
        Self { table }
    }

    pub fn table_ref(&self) -> Arc<Mutex<ChooserTable<T>>> {
        self.table.clone()
    }

    pub fn current_name(&self) -> String {
        let table = self.table.lock().unwrap();
        table.current_name().to_owned()
    }

    pub fn name_vec(&self) -> Vec<String> {
        let table = self.table.lock().unwrap();
        table.name_vec()
    }

    pub fn choice_vec(&self) -> Vec<(String,T)> {
        let table = self.table.lock().unwrap();
        table.choice_vec()
    }

    pub fn update_choice(&mut self, updated_name: &str) {
        let mut table = self.table.lock().unwrap();
        table.choose(updated_name);
    }

    pub fn current_index(&self) -> usize {
        let table = self.table.lock().unwrap();
        table.current_index()
    }
}


#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SynthChoice {
    Original,
    Variation,
}

impl SynthChoice {
    pub fn speaker(&self) -> Speaker {
        match self {
            SynthChoice::Original => HUMAN_SPEAKER,
            SynthChoice::Variation => VARIATION_SPEAKER,
        }
    }
}

pub type SynthTable = ChooserTable<SynthFunc>;

pub fn make_synth_table() -> SynthTable {
    ChooserTable::from(&favorites())
}

pub struct ChooserTable<T: Clone> {
    choices: Vec<(String, T)>,
    name2choice: BTreeMap<String, T>,
    names: Vec<String>,
    current_name: String,
}

impl<T: Clone> ChooserTable<T> {
    pub fn from(choices: &Vec<(String, T)>) -> Self {
        let current_name = choices.iter().next().unwrap().0.to_string();
        let mut name2choice = BTreeMap::new();
        for (name, choice) in choices.iter() {
            name2choice.insert(name.to_string(), choice.clone());
        }
        let names: Vec<String> = choices.iter().map(|c| c.0.to_string()).collect();
        ChooserTable {
            choices: choices.clone(),
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

    pub fn current_index(&self) -> usize {
        self.names
            .iter()
            .enumerate()
            .find(|(_, n)| **n == self.current_name)
            .map(|(i, _)| i)
            .unwrap()
    }

    pub fn name_vec(&self) -> Vec<String> {
        self.names.clone()
    }

    pub fn choice_vec(&self) -> Vec<(String, T)> {
        self.choices.clone()
    }

    pub fn console_pick(&mut self) {
        self.current_name = user_pick_element(self.names.iter().cloned(), |s| s.clone())
    }
}

#[macro_export]
macro_rules! arc_vec {
    ($( ($s:expr, $f:expr)),* ) => {vec![$(($s.to_owned(), Arc::new($f)),)*]}
}

#[derive(Clone)]
pub struct VariationControls {
    pub p_random_slider: Arc<AtomicCell<SliderValue<f64>>>,
    pub p_ornament_slider: Arc<AtomicCell<SliderValue<f64>>>,
    pub whimsify: Arc<AtomicCell<bool>>,
    pub shortest_note_slider: Arc<AtomicCell<SliderValue<f64>>>,
}

impl VariationControls {
    pub fn new() -> Self {
        Self {
            p_random_slider: Arc::new(AtomicCell::new(prob_slider(0.8))),
            p_ornament_slider: Arc::new(AtomicCell::new(prob_slider(0.2))),
            whimsify: Arc::new(AtomicCell::new(false)),
            shortest_note_slider: Arc::new(AtomicCell::new(SliderValue::new(0.1, 0.0, 0.2))),
        }
    }

    pub fn stats(&self, algorithm_name: String) -> VariationStats {
        VariationStats {
            algorithm_name,
            random_prob: self.p_random_slider.load().current,
            ornament_prob: self.p_ornament_slider.load().current,
            min_note_duration: self.shortest_note_slider.load().current,
            whimsify: self.whimsify.load(),
        }
    }

    pub fn update_from(&mut self, stats: &VariationStats) {
        Self::update_slider(self.p_random_slider.clone(), stats.random_prob);
        Self::update_slider(self.p_ornament_slider.clone(), stats.ornament_prob);
        Self::update_slider(self.shortest_note_slider.clone(), stats.min_note_duration);
        self.whimsify.store(stats.whimsify);
    }

    fn update_slider(slider: Arc<AtomicCell<SliderValue<f64>>>, new_val: f64) {
        let mut inner = slider.load();
        inner.set_current(new_val);
        slider.store(inner);
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
        SliderValue {
            current: new_current,
            lo: self.lo,
            hi: self.hi,
        }
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

#[derive(Clone)]
pub struct MelodyRunStatus {
    num_running: Arc<AtomicCell<usize>>,
    stop: Arc<AtomicCell<bool>>,
}

impl MelodyRunStatus {
    pub fn new() -> Self {
        MelodyRunStatus {
            num_running: Arc::new(AtomicCell::new(0)),
            stop: Arc::new(AtomicCell::new(false)),
        }
    }

    pub fn is_stopping(&self) -> bool {
        self.stop.load()
    }

    pub fn send_stop(&self) {
        if self.num_running.load() > 0 {
            self.stop.store(true);
        }
    }

    pub fn report_start(&self) {
        self.num_running.fetch_add(1);
    }

    pub fn report_stop(&self) {
        assert!(self.num_running.load() > 0);
        self.num_running.fetch_sub(1);
        if self.num_running.load() == 0 {
            self.stop.store(false);
        }
    }
}

pub fn send_recorded_melody(
    melody: &Melody,
    speaker: Speaker,
    ai2output: Arc<SegQueue<SynthMsg>>,
    melody_progress: Arc<AtomicCell<Option<f32>>>,
    melody_run_status: MelodyRunStatus,
) {
    melody_run_status.report_start();
    let total_start = Instant::now();
    let total_duration = melody.duration() as f32;
    'outer: for note in melody.iter() {
        let (midi, duration) = note.to_midi();
        ai2output.push(SynthMsg { msg: midi, speaker });
        let note_start = Instant::now();
        while note_start.elapsed().as_secs_f64() < duration {
            let progress = Some(total_start.elapsed().as_secs_f32() / total_duration);
            melody_progress.store(progress);
            if melody_run_status.is_stopping() {
                break 'outer;
            }
        }
    }
    ai2output.push(SynthMsg::all_notes_off(speaker));
    melody_run_status.report_stop();
    melody_progress.store(None);
}

pub fn send_two_melodies(
    melody_left: &Melody,
    melody_right: &Melody,
    ai2output: Arc<SegQueue<SynthMsg>>,
    melody_progress: Arc<AtomicCell<Option<f32>>>,
    melody_run_status: MelodyRunStatus,
) {
    melody_run_status.report_start();
    let elapsed = Instant::now();
    let total_duration = if melody_left.duration() > melody_right.duration() {
        melody_left.duration()
    } else {
        melody_right.duration()
    };
    let mut left_queue = note_times_from(melody_left);
    let mut right_queue = note_times_from(melody_right);
    let mut left_next = 0.0;
    let mut right_next = 0.0;
    loop {
        check_get_next(
            ai2output.clone(),
            elapsed,
            &mut left_next,
            &mut left_queue,
            Speaker::Left,
        );
        check_get_next(
            ai2output.clone(),
            elapsed,
            &mut right_next,
            &mut right_queue,
            Speaker::Right,
        );
        if left_queue.is_empty() && right_queue.is_empty() {
            break;
        } else {
            let progress = Some(elapsed.elapsed().as_secs_f32() / total_duration as f32);
            melody_progress.store(progress);
            if melody_run_status.is_stopping() {
                break;
            }
        }
    }
    ai2output.push(SynthMsg::all_notes_off(Speaker::Both));
    melody_run_status.report_stop();
    melody_progress.store(None);
}

fn check_get_next(
    ai2output: Arc<SegQueue<SynthMsg>>,
    elapsed: Instant,
    next: &mut f64,
    queue: &mut VecDeque<(MidiMsg, f64)>,
    speaker: Speaker,
) {
    if *next < elapsed.elapsed().as_secs_f64() {
        if let Some((msg, time)) = queue.pop_front() {
            *next = time;
            ai2output.push(SynthMsg { msg, speaker });
        }
    }
}

fn note_times_from(melody: &Melody) -> VecDeque<(MidiMsg, f64)> {
    let mut total_time = 0.0;
    let mut result = VecDeque::new();
    for note in melody.iter() {
        let (msg, duration) = note.to_midi();
        total_time += duration;
        result.push_back((msg, total_time));
    }
    result
}
