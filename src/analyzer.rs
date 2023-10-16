use crate::subsequence_finder::{find_maximal_repeated_subs, Subsequences};
use bare_metal_modulo::{MNum, ModNumC, OffsetNumC};
use distribution_select::Distribution;
use enum_iterator::{all, Sequence};
use float_cmp::{ApproxEq, F64Margin};
use histogram_macros::*;
use lazy_static::lazy_static;
use midi_msg::MidiMsg::ChannelVoice;
use midi_msg::{Channel, ChannelVoiceMsg, MidiMsg};
use ordered_float::OrderedFloat;
use rand::prelude::SliceRandom;
use rand::thread_rng;
use std::cmp::{max, min};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::iter::Sum;
use std::ops::RangeInclusive;
use std::ops::{AddAssign, Neg};

pub type MidiByte = i16;

const MAX_MIDI_VALUE: MidiByte = i8::MAX as MidiByte;
const NOTES_PER_OCTAVE: MidiByte = 12;
const USIZE_NOTES_PER_OCTAVE: usize = NOTES_PER_OCTAVE as usize;
const DIATONIC_SCALE_SIZE: usize = 7;
const DIATONIC_SCALE_HOPS: [MidiByte; DIATONIC_SCALE_SIZE] = [2, 2, 1, 2, 2, 2, 1];
const DIATONIC_NOTE_LETTERS: [NoteLetter; DIATONIC_SCALE_SIZE] = [
    NoteLetter::C,
    NoteLetter::D,
    NoteLetter::E,
    NoteLetter::F,
    NoteLetter::G,
    NoteLetter::A,
    NoteLetter::B,
];
const SHARP_START: usize = 3;
const FLAT_START: usize = 6;

const NOTE_IDS: [(NoteLetter, Accidental); USIZE_NOTES_PER_OCTAVE] = [
    (NoteLetter::C, Accidental::Natural),
    (NoteLetter::C, Accidental::Sharp),
    (NoteLetter::D, Accidental::Natural),
    (NoteLetter::D, Accidental::Sharp),
    (NoteLetter::E, Accidental::Natural),
    (NoteLetter::F, Accidental::Natural),
    (NoteLetter::F, Accidental::Sharp),
    (NoteLetter::G, Accidental::Natural),
    (NoteLetter::G, Accidental::Sharp),
    (NoteLetter::A, Accidental::Natural),
    (NoteLetter::A, Accidental::Sharp),
    (NoteLetter::B, Accidental::Natural),
];

const MODE_NAMES: [&str; DIATONIC_SCALE_SIZE] = [
    "Ionian",
    "Dorian",
    "Phrygian",
    "Lydian",
    "Mixolydian",
    "Aeolian",
    "Locrian",
];

pub const FIGURE_LENGTHS: [usize; 2] = [4, 3];

fn major_sharps_for(note_index: usize) -> usize {
    if note_index % 2 == 0 {
        note_index
    } else {
        (note_index + 6) % NOTES_PER_OCTAVE as usize
    }
}

fn major_flats_for(note_index: usize) -> usize {
    (NOTES_PER_OCTAVE as usize + (if note_index % 2 == 0 { 0 } else { 6 }) - note_index)
        % NOTES_PER_OCTAVE as usize
}

fn adjusted_note_index(note_index: usize, mode_index: usize) -> usize {
    let mut note_index = note_index + NOTES_PER_OCTAVE as usize;
    for i in 0..mode_index {
        note_index -= DIATONIC_SCALE_HOPS[i] as usize;
    }
    note_index % NOTES_PER_OCTAVE as usize
}

fn sharps_for(note_index: usize, mode_index: usize) -> usize {
    major_sharps_for(adjusted_note_index(note_index, mode_index))
}

fn flats_for(note_index: usize, mode_index: usize) -> usize {
    major_flats_for(adjusted_note_index(note_index, mode_index))
}

pub fn velocity2volume(midi_velocity: MidiByte) -> f64 {
    max(0, min(MAX_MIDI_VALUE, midi_velocity)) as f64 / MAX_MIDI_VALUE as f64
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Note {
    pitch: MidiByte,
    duration: OrderedFloat<f64>,
    velocity: MidiByte,
}

impl ApproxEq for Note {
    type Margin = F64Margin;

    fn approx_eq<M: Into<Self::Margin>>(self, other: Self, margin: M) -> bool {
        let margin = margin.into();
        self.pitch == other.pitch
            && self.velocity == other.velocity
            && self
                .duration
                .into_inner()
                .approx_eq(other.duration.into_inner(), margin)
    }
}

impl Note {
    pub fn new(pitch: MidiByte, duration: f64, velocity: MidiByte) -> Self {
        Note {
            pitch,
            duration: OrderedFloat(duration),
            velocity,
        }
    }

    pub fn to_midi(&self) -> (MidiMsg, f64) {
        let note = self.pitch as u8;
        let midi = ChannelVoice {
            channel: Channel::Ch1,
            msg: if self.is_rest() {
                ChannelVoiceMsg::NoteOff { note, velocity: 0 }
            } else {
                ChannelVoiceMsg::NoteOn {
                    note,
                    velocity: self.velocity as u8,
                }
            },
        };
        (midi, self.duration.into_inner())
    }

    pub fn pitch(&self) -> MidiByte {
        self.pitch
    }

    pub fn duration(&self) -> f64 {
        self.duration.into_inner()
    }

    pub fn velocity(&self) -> MidiByte {
        self.velocity
    }

    pub fn is_rest(&self) -> bool {
        self.velocity == 0
    }

    pub fn repitched(&self, new_pitch: MidiByte) -> Note {
        Note {
            pitch: new_pitch,
            duration: self.duration,
            velocity: self.velocity,
        }
    }

    pub fn reweigh(&mut self, extra_duration: OrderedFloat<f64>, extra_intensity: MidiByte) {
        let durations = self.duration + extra_duration;
        self.velocity = ((self.duration.into_inner() * self.velocity as f64
            + extra_duration.into_inner() * extra_intensity as f64)
            / durations.into_inner()) as MidiByte;
        self.duration = durations;
    }

    pub fn sonic_pi_list(&self) -> String {
        format!(
            "[{}, {}, {}]",
            self.pitch,
            dotify_float(self.duration),
            dotify_float(OrderedFloat(velocity2volume(self.velocity)))
        )
    }
}

fn dotify_float(f: OrderedFloat<f64>) -> String {
    let mut result = f.to_string();
    if !result.contains(".") {
        result.push_str(".0");
    }
    let (dot_spot, _) = result.chars().enumerate().find(|(_, c)| *c == '.').unwrap();
    while result.len() > dot_spot + 3 {
        result.pop();
    }
    result
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Melody {
    notes: Vec<Note>,
    sections: Vec<Subsequences>,
    figures: Vec<(FigureStart, MelodicFigure)>,
}

impl Melody {
    const MIN_MOTIVE_LEN: usize = 2;
    const MIN_MOTIVE_REPETITIONS: usize = 2;

    pub fn new() -> Self {
        Melody {
            notes: vec![],
            sections: vec![],
            figures: vec![],
        }
    }

    pub fn add(&mut self, n: Note) {
        self.notes.push(n);
        self.identify_sections();
        self.identify_figures();
    }

    fn identify_sections(&mut self) {
        let pitches_only = self.notes.iter().map(|n| n.pitch).collect::<Vec<_>>();
        self.sections = find_maximal_repeated_subs(
            &pitches_only,
            Self::MIN_MOTIVE_REPETITIONS,
            Self::MIN_MOTIVE_LEN,
        );
    }

    fn identify_figures(&mut self) {
        self.figures = vec![];
        let sorted_figures = self.sorted_figures_for();
        for (starts, figure) in sorted_figures.iter() {
            for start in starts.iter() {
                self.figures.push((*start, *figure));
            }
        }
    }

    pub fn sorted_figures_for(&self) -> Vec<(Vec<FigureStart>, MelodicFigure)> {
        MAKER.sorted_figures_for(self)
    }

    pub fn section_number_for(&self, note_index: usize) -> Option<usize> {
        self.sections
            .iter()
            .enumerate()
            .find(|(_, s)| s.contains(note_index))
            .map(|(i, _)| i)
    }

    pub fn from_vec(notes: &Vec<Note>) -> Self {
        let mut result = Self::new();
        result.notes = notes.clone();
        result.identify_sections();
        result
    }

    /// This method exists solely for debugging purposes, to give a more concise
    /// representation than the `Debug` version.
    pub fn tuple_print(&self) {
        for n in self.notes.iter() {
            print!(
                "({}, {}, {}), ",
                n.pitch,
                n.duration.into_inner(),
                n.velocity
            );
        }
        println!();
    }

    pub fn duration(&self) -> f64 {
        self.notes.iter().map(|n| n.duration.into_inner()).sum()
    }

    pub fn num_pitch_changes(&self) -> usize {
        if self.notes.len() == 0 {
            return 0;
        }
        let mut pitch_changes = 0;
        for i in 0..self.notes.len() - 1 {
            if self.notes[i].pitch != self.notes[i + 1].pitch {
                pitch_changes += 1;
            }
        }
        pitch_changes
    }

    pub fn common_prefix_length(&self, other: &Melody) -> usize {
        let mut count = 0;
        for i in 0..min(self.len(), other.len()) {
            if self[i] == other[i] {
                count += 1;
            } else {
                return count;
            }
        }
        count
    }

    pub fn iter(&self) -> impl Iterator<Item = &Note> {
        self.notes.iter().map(|n| n)
    }

    pub fn fragment(&self, start: usize, length: usize) -> Melody {
        let mut result = Self::new();
        for i in start..(start + length) {
            result.notes.push(self.notes[i]);
        }
        result.identify_sections();
        result
    }

    pub fn pitch_subsequence_at(&self, start: usize, length: usize) -> Option<Vec<MidiByte>> {
        let mut result = vec![self.notes[start].pitch];
        for i in start + 1..self.notes.len() {
            if result.len() == length {
                break;
            } else {
                let pitch = self.notes[i].pitch();
                if pitch != *result.last().unwrap() {
                    result.push(pitch);
                }
            }
        }
        if result.len() == length {
            Some(result)
        } else {
            None
        }
    }

    pub fn duration_with_rest(&self, i: usize) -> OrderedFloat<f64> {
        let mut result = self[i].duration;
        let mut i = i + 1;
        while i < self.len() && self[i].is_rest() {
            result += self[i].duration;
            i += 1;
        }
        result
    }

    pub fn median_duration_note_on(&self) -> OrderedFloat<f64> {
        let mut durations = (0..self.len())
            .filter(|i| !self[*i].is_rest())
            .map(|i| self.duration_with_rest(i))
            .collect::<Vec<_>>();
        durations.sort();
        durations[durations.len() / 2]
    }

    pub fn from_str(s: &str) -> Self {
        let mut result = Self::new();
        let mut nums = s.split(",");
        while let Some(note) = nums.next() {
            let note = note.parse().unwrap();
            let duration = nums.next().unwrap().parse().unwrap();
            let f_intensity: OrderedFloat<f64> = nums.next().unwrap().parse().unwrap();
            let intensity = (f_intensity.into_inner() * MAX_MIDI_VALUE as f64) as MidiByte;
            result.add(Note::new(note, duration, intensity));
        }
        result
    }

    pub fn sonic_pi_list(&self) -> String {
        let mut list_str = self
            .notes
            .iter()
            .map(|n| format!("{}, ", n.sonic_pi_list()))
            .collect::<String>();
        list_str.pop();
        list_str.pop();
        format!("[{}]", list_str)
    }

    pub fn last_note(&self) -> Note {
        *self.notes.last().unwrap()
    }

    pub fn len(&self) -> usize {
        self.notes.len()
    }

    pub fn notes_left_from(&self, start: usize) -> usize {
        self.len() - start
    }

    pub fn view_notes(&self) -> String {
        let scale = self.best_scale_for();
        let mut result = String::new();
        for n in self.notes.iter() {
            result.push_str(
                format!(
                    "{} [({:2}) ({:2})] ",
                    scale.note_str(n.pitch),
                    n.duration,
                    n.velocity
                )
                .as_str(),
            );
        }
        result
    }

    pub fn find_note_weights(&self) -> HashMap<MidiByte, OrderedFloat<f64>> {
        assert!(self.len() >= 1);
        let note_iter = self
            .notes
            .iter()
            .map(|n| (n.pitch % NOTES_PER_OCTAVE, n.duration));
        collect_from_by_into!(note_iter, HashMap::new())
    }

    pub fn find_root_pitch(&self) -> MidiByte {
        mode_by_weight!(self.find_note_weights()).unwrap()
    }

    pub fn note_weight_vector(&self) -> [f64; USIZE_NOTES_PER_OCTAVE] {
        assert!(self.len() >= 1);
        let mut result = [0.0; USIZE_NOTES_PER_OCTAVE];
        for n in self.notes.iter() {
            result[n.pitch as usize % USIZE_NOTES_PER_OCTAVE] += n.duration.0;
        }
        result
    }

    pub fn best_scale_for(&self) -> MusicMode {
        let mv = self.note_weight_vector();
        MusicMode::all_major_minor_weights()
            .iter()
            .map(|(mode, v)| (dot_product(v, &mv), mode))
            .max_by_key(|(dp, _)| *dp)
            .map(|(_,m)| m.clone())
            .unwrap()
    }

    pub fn diatonic_intervals(&self) -> Vec<DiatonicInterval> {
        let scale = self.best_scale_for();
        (0..self.notes.len() - 1)
            .map(|i| scale.diatonic_steps_between(self.notes[i].pitch, self.notes[i + 1].pitch))
            .collect()
    }

    /// Returns a Vec containing consolidated `Note` objects.
    /// Two or more `Note` objects are consolidated when they
    /// are consecutive in sequence with the same pitch. The `usize`
    /// is the index of the first note in the consecutive subsequence.
    pub fn get_consolidated_notes(&self) -> Vec<(usize, Note)> {
        if self.notes.len() == 0 {
            vec![]
        } else {
            let mut result = vec![(0, self.notes[0])];
            for (i, note) in self.notes.iter().enumerate().skip(1) {
                if result.last().unwrap().1.pitch == note.pitch {
                    result
                        .last_mut()
                        .unwrap()
                        .1
                        .reweigh(note.duration, note.velocity);
                } else {
                    result.push((i, note.clone()));
                }
            }
            result
        }
    }

    pub fn distinct_consecutive_pitches_in(&self, mut range: RangeInclusive<usize>) -> usize {
        if range.is_empty() {
            return 0;
        }
        let mut count = 1;
        let mut prev = self.notes[range.next().unwrap()];
        for i in range {
            let current = self.notes[i];
            if prev.pitch != current.pitch {
                count += 1;
            }
            prev = current;
        }
        count
    }

    pub fn find_pause_indices(&self) -> Vec<usize> {
        let consolidated = self.get_consolidated_notes();

        (1..(consolidated.len() - 1))
            .filter(|i| {
                consolidated[*i - 1].1.duration < consolidated[*i].1.duration
                    && consolidated[*i].1.duration > consolidated[*i + 1].1.duration
            })
            .map(|i| consolidated[i].0)
            .collect()
    }

    pub fn all_rests_synchronized(&self) -> bool {
        for i in 0..(self.len() - 1) {
            let next = self[i + 1];
            if !self[i].is_rest() && (!next.is_rest() || self[i].pitch() != next.pitch()) {
                return false;
            }
        }
        true
    }

    pub fn synchronize_rests(&mut self) {
        for i in 0..(self.len() - 1) {
            let next = self[i + 1];
            if !self[i].is_rest() && next.pitch() != self[i].pitch {
                let mut j = i + 1;
                while j < self.len() - 1 && self[j].pitch != self[i].pitch {
                    j += 1;
                }
                while j > i + 1 {
                    self.synchro_swap(j, j - 1);
                    j -= 1;
                }
            }
        }
    }

    fn synchro_swap(&mut self, a: usize, b: usize) {
        let pa = self[a].pitch;
        let pb = self[b].pitch;
        self.notes[a].pitch = pb;
        self.notes[b].pitch = pa;
        let av = self[a].velocity;
        let bv = self[b].velocity;
        self.notes[a].velocity = bv;
        self.notes[b].velocity = av;
    }

    pub fn distinct_seq_len(&self, start: usize, num_distinct_pitches: usize) -> Option<usize> {
        let mut seq_len = 1;
        let mut num_distinct_found = 1;
        let mut i = start + 1;
        while i < self.len() {
            if self[i].pitch != self[i - 1].pitch {
                num_distinct_found += 1;
                if num_distinct_found > num_distinct_pitches {
                    break;
                }
            }
            seq_len += 1;
            i += 1;
        }
        if num_distinct_found < num_distinct_pitches {
            None
        } else {
            Some(seq_len)
        }
    }

    pub fn min_max_pitches(&self) -> (MidiByte, MidiByte) {
        let mut lo = self.notes[0].pitch;
        let mut hi = lo;
        for note in self.notes.iter().skip(1) {
            lo = min(lo, note.pitch);
            hi = max(hi, note.pitch);
        }
        (lo, hi)
    }

    pub fn without_brief_notes(&self, min_duration: f64) -> Self {
        let mut result = Melody::new();
        let min_duration = OrderedFloat(min_duration);
        for note in self.iter() {
            if note.duration >= min_duration
                || note.velocity == 0 && result.len() > 0 && result.last_note().pitch == note.pitch
            {
                result.add(*note);
            }
        }
        result
    }

    pub fn notes_ranked_by_duration(&self) -> Vec<(usize, Note)> {
        let mut result = self
            .notes
            .iter()
            .map(|n| n)
            .copied()
            .enumerate()
            .filter(|(_, n)| !n.is_rest())
            .collect::<Vec<_>>();
        result.sort_by(|(_, n1), (_, n2)| n2.duration.cmp(&n1.duration));
        result
    }

    pub fn figure_boundaries(&self) -> VecDeque<(usize, usize, MelodicFigure)> {
        let mut boundaries = self
            .figures
            .iter()
            .map(|(start, f)| (start.start(), start.end(), *f))
            .collect::<Vec<_>>();
        boundaries.sort_by_key(|(s, _, _)| *s);
        boundaries.iter().copied().collect()
    }

    fn randomized_figure_mappings(&self) -> HashMap<MelodicFigure, MelodicFigure> {
        let mut mappings = HashMap::new();
        for (_, figure) in self.figures.iter() {
            if !mappings.contains_key(figure) {
                mappings.insert(*figure, MAKER.pick_figure(*figure));
            }
        }
        mappings
    }

    pub fn generalized_figure_variation(&self) -> Self {
        let mappings = self.randomized_figure_mappings();
        let mut figures2starts = HashMap::new();
        for (start, figure) in self.figures.iter() {
            match figures2starts.get_mut(figure) {
                None => {
                    figures2starts.insert(figure, vec![*start]);
                }
                Some(v) => {
                    v.push(*start);
                }
            }
        }
        let mut all_source_figures = mappings.keys().collect::<Vec<_>>();
        all_source_figures.shuffle(&mut thread_rng());
        let mut result = self.clone();
        let mut replaced_indices = HashSet::new();
        while all_source_figures.len() > 0 {
            let target = all_source_figures.pop().unwrap();
            let replacer = mappings.get(target).unwrap();
            for start in figures2starts.get(target).unwrap().iter() {
                let mut replacements = self.replacements(start.start(), start.end(), replacer);
                let mut already_replaced = false;
                for (r, _) in replacements.iter() {
                    if replaced_indices.contains(r) {
                        already_replaced = true;
                    }
                }
                if !already_replaced {
                    while replacements.len() > 0 {
                        let (i, note) = replacements.pop_front().unwrap();
                        result.notes[i] = note;
                        replaced_indices.insert(i);
                    }
                }
            }
        }
        result.identify_figures();
        result
    }

    fn replacements(
        &self,
        start: usize,
        end: usize,
        replacer: &MelodicFigure,
    ) -> VecDeque<(usize, Note)> {
        let scale = self.best_scale_for();
        let mut pitch_queue = replacer.make_pitches(self.notes[start].pitch, &scale);
        pitch_queue.pop_front();
        pitch_queue.pop_back();
        let mut result = VecDeque::new();
        let mut i = start + 1;
        while i < end && self.notes[i - 1].pitch == self.notes[i].pitch {
            i += 1;
        }
        while i < end && pitch_queue.len() > 0 {
            result.push_back((i, self.notes[i].repitched(pitch_queue[0])));
            i += 1;
            if self.notes[i].pitch != self.notes[i - 1].pitch {
                pitch_queue.pop_front();
            }
        }
        result
    }

    pub fn figure_swap_variation(&self) -> Self {
        let mappings = self.randomized_figure_mappings();
        let scale = self.best_scale_for();
        let mut result = Self::new();
        result.figures = self
            .figures
            .iter()
            .map(|(s, f)| (*s, *mappings.get(f).unwrap()))
            .collect();
        let mut i = 0;
        let mut f = 0;
        while i < self.len() {
            if f < result.figures.len() && result.figures[f].0.start() == i {
                let mut pitch_queue = result.figures[f]
                    .1
                    .make_pitches(self.notes[i].pitch, &scale);
                while !pitch_queue.is_empty()
                    && (f + 1 == result.figures.len() || i < result.figures[f + 1].0.start())
                {
                    result.notes.push(self.notes[i].repitched(pitch_queue[0]));
                    i += 1;
                    if i == self.notes.len() || self.notes[i].pitch != self.notes[i - 1].pitch {
                        pitch_queue.pop_front();
                    }
                }
                f += 1;
            } else {
                result.notes.push(self.notes[i]);
                i += 1;
            }
        }
        result.identify_sections();
        result
    }
}

impl std::ops::Index<usize> for Melody {
    type Output = Note;

    fn index(&self, index: usize) -> &Self::Output {
        &self.notes[index]
    }
}

impl std::ops::IndexMut<usize> for Melody {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.notes[index]
    }
}

lazy_static! {
    static ref MAKER: MelodyMaker = MelodyMaker::new();
}

pub struct MelodyMaker {
    figure_tables: BTreeMap<usize, BTreeMap<MidiByte, Vec<MelodicFigure>>>,
    all_figures: HashSet<MelodicFigure>,
}

impl MelodyMaker {
    pub fn new() -> Self {
        let figure_tables: BTreeMap<usize, BTreeMap<MidiByte, Vec<MelodicFigure>>> = FIGURE_LENGTHS
            .iter()
            .map(|len| (*len, MelodicFigure::interval2figures(*len)))
            .collect();

        let mut all_figures = HashSet::new();
        for table in figure_tables.values() {
            for fig_list in table.values() {
                for fig in fig_list.iter() {
                    all_figures.insert(*fig);
                }
            }
        }

        MelodyMaker {
            figure_tables,
            all_figures,
        }
    }

    pub fn make_figure_distribution(&self, melody: &Melody) -> Distribution<MelodicFigure> {
        let mut result = self.make_uniform_figure_distribution();
        for (_, figure, _) in self.all_figure_matches(melody) {
            result.add(&figure, 1.0);
        }
        result
    }

    pub fn make_uniform_figure_distribution(&self) -> Distribution<MelodicFigure> {
        let mut result = Distribution::new();
        for table in self.figure_tables.values() {
            for figures in table.values() {
                for figure in figures.iter() {
                    result.add(figure, 1.0);
                }
            }
        }
        result
    }

    pub fn sorted_figures_for(&self, melody: &Melody) -> Vec<(Vec<FigureStart>, MelodicFigure)> {
        let scale = melody.best_scale_for();
        let mut figure_starts = MAKER
            .all_figures
            .iter()
            .map(|f| (f.all_match_starts_in(melody, &scale), *f))
            .filter(|(v, _)| v.len() > 0)
            .collect::<Vec<_>>();
        figure_starts = Self::purge_contained_starts(&figure_starts);
        figure_starts.sort_by_key(|k| -(k.0.len() as isize));
        figure_starts
    }

    fn purge_contained_starts(
        starts: &Vec<(Vec<FigureStart>, MelodicFigure)>,
    ) -> Vec<(Vec<FigureStart>, MelodicFigure)> {
        let mut result = vec![];
        for (fig_starts, fig) in starts.iter() {
            let usable_starts = fig_starts
                .iter()
                .filter(|s| !Self::contained_by_any(**s, starts))
                .copied()
                .collect::<Vec<_>>();
            if usable_starts.len() > 0 {
                result.push((usable_starts, *fig));
            }
        }
        result
    }

    fn contained_by_any(
        start: FigureStart,
        starts: &Vec<(Vec<FigureStart>, MelodicFigure)>,
    ) -> bool {
        for (fig_starts, _) in starts.iter() {
            for fig_start in fig_starts.iter() {
                if start.overlap_with(fig_start) == FigureStartOverlap::ContainedBy {
                    return true;
                }
            }
        }
        false
    }

    /// Finds a MelodicFigure that matches the pitch sequence in `melody` starting at `start`.
    /// Prefers matching a 4-note figure to a 3-note figure.
    pub fn matching_figure(&self, melody: &Melody, start: usize) -> Option<(MelodicFigure, usize)> {
        let scale = melody.best_scale_for();
        for length in FIGURE_LENGTHS.iter() {
            if let Some(pitches) = melody.pitch_subsequence_at(start, *length) {
                if let Some(step_gap) = scale
                    .diatonic_steps_between(*pitches.first().unwrap(), *pitches.last().unwrap())
                    .pure_degree()
                {
                    if let Some(candidates) = self.figure_candidates(*length, step_gap) {
                        for candidate in candidates.iter() {
                            if let Some(match_len) = candidate.match_length(melody, &scale, start) {
                                return Some((*candidate, match_len));
                            }
                        }
                    }
                }
            }
        }
        None
    }

    pub fn figure_candidates(
        &self,
        figure_length: usize,
        step_gap: MidiByte,
    ) -> Option<&Vec<MelodicFigure>> {
        self.figure_tables
            .get(&figure_length)
            .unwrap()
            .get(&step_gap)
    }

    pub fn ornamented(&self, scale: &MusicMode, melody: &Melody, p_ornament: f64) -> Melody {
        if melody.len() == 0 {
            return melody.clone();
        }
        let mut rng = rand::thread_rng();
        let consolidated = melody.get_consolidated_notes();
        let ornament_duration = consolidated.iter().map(|(_, n)| n.duration).min().unwrap();

        let mut result = Melody::new();
        let mut i = 0;
        while i < melody.len() {
            let mut ornamented = false;
            if let Some(gap) =
                Self::next_diatonic_gap(&scale, melody, i).and_then(|g| g.pure_degree())
            {
                if let Some((figure_length, lead_duration)) =
                    Self::ornament_figure_length(ornament_duration, melody, i)
                {
                    if let Some(figure_list) =
                        self.figure_tables.get(&figure_length).unwrap().get(&gap)
                    {
                        if Self::any_notes_after(melody, i) && rand::random::<f64>() < p_ornament {
                            let figure = figure_list.choose(&mut rng).unwrap();
                            let mut pitches = figure.make_pitches(melody[i].pitch, &scale);
                            pitches.pop_back();
                            let mut duration = vec![lead_duration.into_inner()];
                            while !pitches.is_empty() {
                                let p = pitches.pop_front().unwrap();
                                result.add(Note::new(
                                    p,
                                    duration.pop().unwrap_or(ornament_duration.into_inner()),
                                    melody[i].velocity,
                                ));
                                result.add(Note::new(p, 0.0, 0));
                            }
                            ornamented = true;
                            i += 1;
                            while i < melody.len() && melody[i].is_rest() {
                                i += 1;
                            }
                        }
                    }
                }
            }
            if !ornamented {
                result.add(melody[i]);
                i += 1;
            }
        }
        result
    }

    fn ornament_figure_length(
        ornament_duration: OrderedFloat<f64>,
        melody: &Melody,
        i: usize,
    ) -> Option<(usize, OrderedFloat<f64>)> {
        if !melody[i].is_rest() {
            let duration = melody.duration_with_rest(i);
            for len in FIGURE_LENGTHS {
                let leftover = duration - ornament_duration * (OrderedFloat(len as f64) - 2.0);
                if leftover >= OrderedFloat(0.0) {
                    return Some((len, leftover));
                }
            }
        }
        None
    }

    fn any_notes_after(melody: &Melody, start: usize) -> bool {
        (start..melody.len()).any(|i| !melody[i].is_rest())
    }

    fn next_diatonic_gap(scale: &MusicMode, melody: &Melody, i: usize) -> Option<DiatonicInterval> {
        let next = if i + 1 < melody.len() && !melody[i + 1].is_rest() {
            Some(i + 1)
        } else if i + 2 < melody.len() {
            Some(i + 2)
        } else {
            None
        };
        next.map(|n| scale.diatonic_steps_between(melody[i].pitch, melody[n].pitch))
    }

    pub fn all_figure_matches(&self, melody: &Melody) -> Vec<(usize, MelodicFigure, usize)> {
        (0..melody.len())
            .filter_map(|i| self.matching_figure(melody, i).map(|f| (i, f.0, f.1)))
            .collect()
    }

    pub fn pick_figure(&self, figure: MelodicFigure) -> MelodicFigure {
        let figure_length = figure.len();
        let jump = figure.total_diatonic_change();
        let table = self.figure_tables.get(&figure_length).unwrap();
        let options = table
            .get(&jump)
            .unwrap()
            .iter()
            .filter(|f| **f != figure)
            .copied()
            .collect::<Vec<_>>();
        if options.len() > 0 {
            options[rand::random::<usize>() % options.len()]
        } else {
            figure
        }
    }

    const MIN_MOTIVE_LEN: usize = 2;
    const MIN_MOTIVE_REPETITIONS: usize = 2;

    pub fn create_remapped_variation(&self, original: &Melody, _p: f64) -> Melody {
        original.figure_swap_variation()
    }

    pub fn create_remapped2_variation(&self, original: &Melody, _p: f64) -> Melody {
        original.generalized_figure_variation()
    }

    pub fn create_motive_variation(&self, original: &Melody, p_remap: f64) -> Melody {
        let scale = original.best_scale_for();
        let mut sections = self.get_melody_sections(original);
        for section in sections.iter_mut() {
            section.vary(&scale, p_remap, self);
        }
        let mut variation = original.clone();
        for section in sections.iter() {
            section.remelodize(&mut variation);
        }
        variation
    }

    pub fn get_melody_sections(&self, melody: &Melody) -> Vec<MelodySection> {
        let consolidated = melody.get_consolidated_notes();
        let consolidated_melody = Melody::from_vec(&consolidated.iter().map(|(_, n)| *n).collect());
        let intervals = consolidated_melody.diatonic_intervals();
        let subs = find_maximal_repeated_subs(
            &intervals,
            Self::MIN_MOTIVE_REPETITIONS,
            Self::MIN_MOTIVE_LEN,
        );
        MelodySection::from(&subs, &intervals, &consolidated)
    }

    pub fn create_whimsical_variation(&self, original: &Melody, p_go_back: f64) -> Melody {
        // Algorithm:
        //
        // 1. Start at original pitch.
        // 2. For each pitch:
        // * Resample our distribution based on whether we are going towards or away.
        // 3. Keep going until we have exhausted the figure.
        //   * Variation: Include a chance of picking a new figure that "fits" the current one.
        // 4. Pick a new figure once we're done.
        // 5. p_go_back increases with each interval we pick. It resets when we get back to an original note.
        //   * Idea: Maybe p_go_back can be that interval, and it always just starts at zero.
        let scale = original.best_scale_for();
        let distro = self.make_figure_distribution(original);
        let mut p_back = 0.0;
        let mut variation = Melody::new();
        let mut pitch_queue: VecDeque<MidiByte> = VecDeque::new();
        variation.add(original[0]);
        for i in 1..original.len() {
            if original[i].pitch() == original[i - 1].pitch() {
                variation.add(original[i].repitched(variation.last_note().pitch()));
            } else {
                let popped = pitch_queue.pop_front().unwrap_or_else(|| {
                    let target_direction = if rand::random::<f64>() < 0.5 {
                        MelodyDirection::Toward
                    } else {
                        p_back += p_go_back;
                        MelodyDirection::Away
                    };

                    let reduced_distro = distro.distro_with(|f| {
                        MelodyDirection::find(*f, original[i].pitch(), &original, i)
                            == target_direction
                    });
                    let figure = (if reduced_distro.is_empty() {
                        &distro
                    } else {
                        &reduced_distro
                    })
                    .random_pick();
                    let start_pitch = variation.last_note().pitch();
                    for pitch in figure.make_pitches(start_pitch, &scale).iter().skip(1) {
                        pitch_queue.push_back(*pitch);
                    }
                    pitch_queue.pop_front().unwrap()
                });
                if popped == original[i].pitch() {
                    p_back = 0.0;
                }
                variation.add(original[i].repitched(popped));
            }
        }
        variation
    }

    pub fn randomize_subsection(&self, melody: &mut Melody, subrange: RangeInclusive<usize>) {
        let scale = melody.best_scale_for();
        let distro = self.make_figure_distribution(melody);
        let mut start = *subrange.start();
        let end = *subrange.end();
        loop {
            let pitches_left = melody.distinct_consecutive_pitches_in(start..=end);
            let figure_len: usize = match pitches_left {
                0..=2 => break,
                3 | 5 => 3,
                4 => 4,
                _ => {
                    if rand::random::<f64>() < 0.33 {
                        3
                    } else {
                        4
                    }
                }
            };
            let reduced_distro = distro.distro_with(|f| {
                let mut matches = figure_len == f.len();
                if figure_len <= 4 {
                    let pitches = f.make_pitches(melody[start].pitch(), &scale);
                    matches = matches
                        && pitches.back().unwrap() % NOTES_PER_OCTAVE
                            == melody[end].pitch() % NOTES_PER_OCTAVE;
                }
                matches
            });
            let figure = (if reduced_distro.is_empty() {
                &distro
            } else {
                &reduced_distro
            })
            .random_pick();
            let mut pitches = figure.make_pitches(melody[start].pitch(), &scale);
            while let Some(new_pitch) = pitches.pop_front() {
                let original = melody[start].pitch();
                while start < melody.len() && melody[start].pitch() == original {
                    melody[start] = melody[start].repitched(new_pitch); // TODO: This is bad - the approach I now have in mind is to mediate all Melody alterations through the as-yet-nonexistent auto-annotator.
                    start += 1;
                }
            }
            start -= 1;
        }
    }

    pub fn create_wandering_variation(&self, original: &Melody, p_eliminate: f64) -> Melody {
        let mut ranking = original.notes_ranked_by_duration();
        let target_len = (ranking.len() as f64 * (1.0 - p_eliminate)) as usize;
        while ranking.len() > target_len {
            ranking.pop();
        }
        let mut ranking: BTreeMap<usize, Note> = ranking.iter().copied().collect();
        ranking.insert(0, original[0]);
        ranking.insert(original.len() - 1, original[original.len() - 1]);
        let mut variation = original.clone();
        let mut prev = 0;
        for i in ranking.keys() {
            self.randomize_subsection(&mut variation, prev..=*i);
            prev = *i;
        }
        variation
    }

    pub fn whimsified_ending(&self, original: &Melody) -> Melody {
        let distro = self.make_figure_distribution(original);
        let whimsifier = distro.random_pick();
        let mut result = original.clone();
        let mut countdown = whimsifier.len();
        let mut i = original.len();
        while countdown > 0 {
            if i == 0 {
                return result;
            }
            i -= 1;
            if !result[i].is_rest() {
                countdown -= 1;
            }
        }
        let mut pitches = whimsifier.make_pitches(original[i].pitch(), &original.best_scale_for());
        let mut i = original.len();
        while pitches.len() > 0 {
            i -= 1;
            let pitch = if result[i].is_rest() {
                pitches[pitches.len() - 1]
            } else {
                pitches.pop_back().unwrap()
            };
            result[i] = result[i].repitched(pitch); // TODO: Again, this is bad.
        }
        result
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MelodySection {
    intervals: Vec<DiatonicInterval>,
    starts: Vec<usize>,
}

impl MelodySection {
    pub fn from(
        subs: &Vec<Subsequences>,
        intervals: &Vec<DiatonicInterval>,
        consolidated: &Vec<(usize, Note)>,
    ) -> Vec<Self> {
        let mut result = vec![];
        for sub in subs.iter() {
            result.push(Self::sub2section(sub, intervals, consolidated));
        }
        Self::intersperse_missing_sections(&mut result, intervals, consolidated);
        result
    }

    fn sub2section(
        sub: &Subsequences,
        intervals: &Vec<DiatonicInterval>,
        consolidated: &Vec<(usize, Note)>,
    ) -> Self {
        let starts: Vec<usize> = sub.starts().iter().map(|s| consolidated[*s].0).collect();
        let start = sub.starts().iter().next().copied().unwrap();
        let intervals: Vec<DiatonicInterval> =
            (0..(sub.sub_len())).map(|i| intervals[i + start]).collect();
        MelodySection { intervals, starts }
    }

    fn intersperse_missing_sections(
        sections: &mut Vec<Self>,
        intervals: &Vec<DiatonicInterval>,
        consolidated: &Vec<(usize, Note)>,
    ) {
        let uncovered = Self::find_uncovered_indices(sections, consolidated);
        Self::intersperse_uncovered(sections, intervals, &uncovered, consolidated);
    }

    fn find_uncovered_indices(
        sections: &Vec<Self>,
        consolidated: &Vec<(usize, Note)>,
    ) -> BTreeSet<usize> {
        let mut uncovered = (0..consolidated.len() - 1).collect::<BTreeSet<usize>>();
        for section in sections.iter() {
            for start in section.starts.iter() {
                let covered = consolidated
                    .iter()
                    .enumerate()
                    .find(|(_, (i, _))| i == start)
                    .unwrap()
                    .0;
                for i in 0..section.intervals.len() {
                    uncovered.remove(&(covered + i));
                }
            }
        }
        uncovered
    }

    fn intersperse_uncovered(
        sections: &mut Vec<Self>,
        intervals: &Vec<DiatonicInterval>,
        uncovered: &BTreeSet<usize>,
        consolidated: &Vec<(usize, Note)>,
    ) {
        let mut uncoverer = uncovered.iter();
        if let Some(mut prev) = uncoverer.next().copied() {
            let mut current = Self {
                intervals: vec![intervals[prev]],
                starts: vec![consolidated[prev].0],
            };
            for i in uncoverer.copied() {
                if i == prev + 1 {
                    current.intervals.push(intervals[i]);
                } else {
                    let mut adding = Self {
                        intervals: vec![intervals[i]],
                        starts: vec![consolidated[i].0],
                    };
                    std::mem::swap(&mut adding, &mut current);
                    if adding.intervals.len() >= MelodyMaker::MIN_MOTIVE_LEN {
                        sections.push(adding);
                    }
                }
                prev = i;
            }
            sections.push(current);
        }
    }

    pub fn overall_interval_change(&self, scale: &MusicMode) -> DiatonicInterval {
        self.intervals
            .iter()
            .copied()
            .sum::<DiatonicInterval>()
            .normalized(scale)
    }

    pub fn vary(&mut self, scale: &MusicMode, replace_prob: f64, maker: &MelodyMaker) {
        let mut rng = rand::thread_rng();
        let mut i = 0;
        loop {
            let figure_length = FIGURE_LENGTHS.choose(&mut rng).unwrap();
            let figure_end = i + figure_length - 2;
            if figure_end >= self.intervals.len() {
                break;
            }
            if rand::random::<f64>() < replace_prob {
                self.figure_replace(maker, &mut i, scale, *figure_length, figure_end);
            }
            i += 1;
        }
    }

    fn figure_replace(
        &mut self,
        maker: &MelodyMaker,
        i: &mut usize,
        scale: &MusicMode,
        figure_length: usize,
        figure_end: usize,
    ) {
        let mut rng = rand::thread_rng();
        let current_intervals = &self.intervals[*i..=figure_end];
        if let Some(step_gap) = current_intervals
            .iter()
            .copied()
            .sum::<DiatonicInterval>()
            .normalized(scale)
            .pure_degree()
        {
            if let Some(figure_candidates) = maker.figure_candidates(figure_length, step_gap) {
                let current_diatonic = current_intervals
                    .iter()
                    .map(|i| i.degree)
                    .collect::<Vec<_>>();
                let figure_candidates = figure_candidates
                    .iter()
                    .filter(|f| f.pattern() != current_diatonic)
                    .collect::<Vec<_>>();
                if let Some(replacement) = figure_candidates.choose(&mut rng).copied() {
                    for (j, interval) in replacement.pattern().iter().enumerate() {
                        self.intervals[*i + j] = DiatonicInterval::pure(*interval);
                    }
                    *i = figure_end;
                }
            }
        }
    }

    pub fn remelodize(&self, melody: &mut Melody) {
        let scale = melody.best_scale_for();
        for start in self.starts.iter() {
            self.remelodize_at(melody, *start, &scale);
        }
    }

    fn remelodize_at(&self, melody: &mut Melody, start: usize, scale: &MusicMode) {
        let mut pitch = melody[start].pitch;
        let mut i = 0;
        let mut m = start;
        loop {
            let last_pitch = melody[m].pitch;
            melody[m].pitch = pitch; // TODO: This is bad. Invalidates the annotation.
            m += 1;
            if m >= melody.len() {
                break;
            }
            if last_pitch != melody[m].pitch {
                pitch = scale.next_pitch(pitch, self.intervals[i]);
                i += 1;
                if i >= self.intervals.len() {
                    break;
                }
            }
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct DiatonicInterval {
    degree: MidiByte,
    chroma: MidiByte,
}

impl DiatonicInterval {
    pub fn new() -> Self {
        DiatonicInterval {
            degree: 0,
            chroma: 0,
        }
    }

    pub fn pure(degree: MidiByte) -> Self {
        DiatonicInterval { degree, chroma: 0 }
    }

    pub fn chromatic(degree: MidiByte, chroma: MidiByte) -> Self {
        DiatonicInterval { degree, chroma }
    }

    pub fn normalized(&self, scale: &MusicMode) -> Self {
        scale.diatonic_steps_between(scale.root(), scale.next_pitch(scale.root(), *self))
    }

    pub fn pure_degree(&self) -> Option<MidiByte> {
        if self.chroma == 0 {
            Some(self.degree)
        } else {
            None
        }
    }
}

impl AddAssign for DiatonicInterval {
    fn add_assign(&mut self, rhs: Self) {
        self.degree += rhs.degree;
        self.chroma += rhs.chroma;
    }
}

impl Sum for DiatonicInterval {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut result = DiatonicInterval::new();
        for item in iter {
            result += item;
        }
        result
    }
}

impl Neg for DiatonicInterval {
    type Output = DiatonicInterval;

    fn neg(self) -> Self::Output {
        Self {
            degree: -self.degree,
            chroma: -self.chroma,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence)]
pub enum NoteLetter {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
}

impl NoteLetter {
    pub fn next(&self) -> Self {
        enum_iterator::next_cycle(self).unwrap()
    }

    pub fn prev(&self) -> Self {
        enum_iterator::previous_cycle(self).unwrap()
    }

    pub fn natural_pitch(&self) -> MidiByte {
        match self {
            NoteLetter::A => 9,
            NoteLetter::B => 11,
            NoteLetter::C => 0,
            NoteLetter::D => 2,
            NoteLetter::E => 4,
            NoteLetter::F => 5,
            NoteLetter::G => 7,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum MelodyDirection {
    Toward,
    Away,
}

impl MelodyDirection {
    fn find(
        figure: MelodicFigure,
        start_pitch: MidiByte,
        target_melody: &Melody,
        target_start: usize,
    ) -> Self {
        let scale = target_melody.best_scale_for();
        let mut figure_pitches = figure.make_pitches(start_pitch, &scale);
        let final_pitch = figure_pitches.back().copied().unwrap();
        let mut i = target_start;
        loop {
            if i == 0 || target_melody[i - 1].pitch() != target_melody[i].pitch() {
                figure_pitches.pop_front();
            }
            if i + 1 == target_melody.len() || figure_pitches.len() == 0 {
                break;
            } else {
                i += 1;
            }
        }
        let target_pitch = target_melody[i].pitch();
        let start_pitch_distance = (start_pitch - target_pitch).abs();
        let figure_pitch_distance = (target_pitch - final_pitch).abs();
        if figure_pitch_distance < start_pitch_distance {
            Self::Toward
        } else {
            Self::Away
        }
    }
}

// From http://davidtemperley.com/wp-content/uploads/2015/11/temperley-mp99.pdf
const TEMPERLEY_C_MAJOR: [f64; 12] = [5.0, 2.0, 3.5, 2.0, 4.5, 4.0, 2.0, 4.5, 2.0, 3.5, 1.5, 4.0];
const TEMPERLEY_C_MINOR: [f64; 12] = [5.0, 2.0, 3.5, 4.5, 2.0, 4.0, 2.0, 4.5, 3.5, 2.0, 1.5, 4.0];

#[derive(Copy, Clone, Eq, PartialEq, Debug, Ord, PartialOrd)]
pub struct MusicMode {
    root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>,
    octave_notes: [ModNumC<MidiByte, USIZE_NOTES_PER_OCTAVE>; DIATONIC_SCALE_SIZE],
    intervals: [MidiByte; DIATONIC_SCALE_SIZE],
}

fn dot_product(
    v1: &[f64; USIZE_NOTES_PER_OCTAVE],
    v2: &[f64; USIZE_NOTES_PER_OCTAVE],
) -> OrderedFloat<f64> {
    let mut sum = 0.0;
    for i in 0..USIZE_NOTES_PER_OCTAVE {
        sum += v1[i] * v2[i];
    }
    OrderedFloat(sum)
}

impl MusicMode {
    pub fn all_modes_for(root_note: MidiByte) -> Vec<Self> {
        (0..DIATONIC_SCALE_SIZE)
            .map(|i| Self::new(ModNumC::new(i), root_note))
            .collect()
    }

    pub fn all_major_minor_weights() -> Vec<(Self, [f64; USIZE_NOTES_PER_OCTAVE])> {
        let mut result = vec![];
        for i in 0..USIZE_NOTES_PER_OCTAVE {
            let mut major = [0.0; USIZE_NOTES_PER_OCTAVE];
            let mut minor = [0.0; USIZE_NOTES_PER_OCTAVE];
            for n in 0..USIZE_NOTES_PER_OCTAVE {
                let dest_n = ModNumC::<usize,USIZE_NOTES_PER_OCTAVE>::new(n + i);
                major[dest_n.a()] = TEMPERLEY_C_MAJOR[n];
                minor[dest_n.a()] = TEMPERLEY_C_MINOR[n];
            }
            result.push((Self::new(ModNumC::new(0), i as MidiByte), major));
            result.push((Self::new(ModNumC::new(5), i as MidiByte), minor));
        }
        result
    }

    pub fn root_name(&self) -> (NoteLetter, Accidental) {
        let (mut note, mut acc) = NOTE_IDS[self.root() as usize % USIZE_NOTES_PER_OCTAVE];
        if acc == Accidental::Sharp && self.is_flat_key() {
            note = note.next();
            acc = Accidental::Flat;
        }
        (note, acc)
    }

    pub fn note_names(&self) -> [(NoteLetter, Accidental); DIATONIC_SCALE_SIZE] {
        let root = self.root_name();
        let mut result = [root; DIATONIC_SCALE_SIZE];
        for i in 1..result.len() {
            let note = result[i - 1].0.next();
            let last_pitch = result[i - 1].0.natural_pitch() + result[i - 1].1.pitch_shift();
            let mut pitch = note.natural_pitch();
            if pitch < last_pitch {
                pitch += NOTES_PER_OCTAVE;
            }
            result[i] = (note, match last_pitch + self.intervals[i - 1] - pitch {
                -1 => Accidental::Flat,
                0 => Accidental::Natural,
                1 => Accidental::Sharp,
                _value => panic!("Pitch distance: {_value} from {:?} to {:?} via {}; shouldn't happen with a diatonic scale!", result[i - 1], note, self.intervals[i - 1])
            });
        }
        result
    }

    pub fn name(&self) -> String {
        let (note, acc) = self.root_name();
        format!(
            "{} {}",
            Self::format_note(note, acc),
            MODE_NAMES[self.root_pos.a()]
        )
    }

    pub fn format_note(note: NoteLetter, acc: Accidental) -> String {
        let symbol = if acc == Accidental::Natural {
            "".to_string()
        } else {
            acc.symbol().to_string()
        };
        format!("{note:?}{}", symbol)
    }

    pub fn new(root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>, root_note: MidiByte) -> Self {
        let mut octave_notes = [ModNumC::new(root_note); DIATONIC_SCALE_SIZE];
        let mut offset = DIATONIC_SCALE_HOPS[root_pos.a()];
        for i in root_pos.iter().skip(1) {
            octave_notes[i.a()] += offset;
            offset += DIATONIC_SCALE_HOPS[i.a()];
        }
        let mut intervals = [0; DIATONIC_SCALE_SIZE];
        for (i, p) in root_pos.iter().enumerate() {
            intervals[i] = DIATONIC_SCALE_HOPS[p.a()];
        }
        MusicMode {
            root_pos,
            octave_notes,
            intervals,
        }
    }

    pub fn c_value(&self) -> MidiByte {
        match sharps_for(self.root() as usize, self.root_pos.a()) {
            6 => 59,
            2..=5 => 61,
            _ => 60,
        }
    }

    pub fn diatonic_steps_between_round_up(&self, pitch1: MidiByte, pitch2: MidiByte) -> MidiByte {
        self.diatonic_steps_between(
            self.closest_pitch_below(pitch1),
            self.closest_pitch_above(pitch2),
        )
        .pure_degree()
        .unwrap()
    }

    pub fn diatonic_steps_between(&self, pitch1: MidiByte, pitch2: MidiByte) -> DiatonicInterval {
        assert!(pitch1 < i8::MAX as MidiByte + NOTES_PER_OCTAVE);
        if pitch1 > pitch2 {
            -self.diatonic_steps_between(pitch2, pitch1)
        } else {
            let steps_before = self.half_steps_up_to_scale(pitch1);
            let mut count = 0;
            let mut p = pitch1 + steps_before;
            while p < self.closest_pitch_below(pitch2) {
                p = self.next_pitch(p, DiatonicInterval::pure(1));
                count += 1;
            }
            let steps_after = self.half_steps_up_to_scale(pitch2);
            DiatonicInterval {
                degree: count,
                chroma: steps_before + steps_after,
            }
        }
    }

    /// Returns the diatonic pitch `scale_steps_away` from `reference_pitch`.
    /// If `reference_pitch` is not a member of `self`'s scale:
    /// * First, it adds the `chroma` value from `scale_steps_away`, and sees if
    ///   the resulting pitch is now a member of the scale. As part of that process,
    ///   it resets the `chroma` to zero in `scale_steps_away`.
    /// * Next, if that adjustment fails, it rounds the adjusted `reference_pitch` up to
    ///   the next pitch that is within the scale.
    pub fn next_pitch(
        &self,
        reference_pitch: MidiByte,
        scale_steps_away: DiatonicInterval,
    ) -> MidiByte {
        assert!(reference_pitch < MidiByte::MAX);
        let (reference_pitch, scale_steps_away) =
            self.reference_on_scale(reference_pitch, scale_steps_away);
        let mut octaves_up = num::integer::div_floor(reference_pitch, NOTES_PER_OCTAVE);
        let i = self.closest_position_for(reference_pitch);
        let ref_octave_basis = self.octave_notes[i].a();
        let j: ModNumC<MidiByte, DIATONIC_SCALE_SIZE> =
            ModNumC::new(i as MidiByte + scale_steps_away.degree);
        let next_octave_basis = self.octave_notes[j.a() as usize].a();
        if scale_steps_away.degree > 0 && ref_octave_basis > next_octave_basis {
            octaves_up += 1
        } else if scale_steps_away.degree < 0 && ref_octave_basis < next_octave_basis {
            octaves_up -= 1;
        }
        octaves_up += scale_steps_away.degree / 7;
        scale_steps_away.chroma + next_octave_basis + octaves_up * NOTES_PER_OCTAVE
    }

    fn reference_on_scale(
        &self,
        reference_pitch: MidiByte,
        scale_steps_away: DiatonicInterval,
    ) -> (MidiByte, DiatonicInterval) {
        if self.contains(reference_pitch) {
            (reference_pitch, scale_steps_away)
        } else {
            (
                reference_pitch + scale_steps_away.chroma,
                DiatonicInterval::pure(scale_steps_away.degree),
            )
        }
    }

    pub fn closest_position_for(&self, reference_pitch: MidiByte) -> usize {
        let mut reference_pitch = reference_pitch;
        loop {
            if let Some(position) = self.octave_notes.iter().position(|p| *p == reference_pitch) {
                return position;
            } else {
                reference_pitch += 1;
            }
        }
    }

    pub fn closest_pitch_below(&self, pitch: MidiByte) -> MidiByte {
        self.closest_pitch(pitch, -1)
    }

    pub fn closest_pitch_above(&self, pitch: MidiByte) -> MidiByte {
        self.closest_pitch(pitch, 1)
    }

    fn closest_pitch(&self, pitch: MidiByte, step: MidiByte) -> MidiByte {
        let mut pitch = pitch;
        while !self.contains(pitch) {
            pitch += step;
        }
        pitch
    }

    fn half_steps_up_to_scale(&self, pitch: MidiByte) -> MidiByte {
        let mut count = 0;
        while !self.contains(pitch + count) {
            count += 1;
        }
        count
    }

    fn root(&self) -> MidiByte {
        self.octave_notes[self.root_pos.a()].a()
    }

    pub fn contains(&self, pitch: MidiByte) -> bool {
        self.octave_notes.contains(&(ModNumC::new(pitch)))
    }

    pub fn diatonic_degree(&self, pitch: MidiByte) -> DiatonicInterval {
        let pertinent_root = self.find_closest_root_beneath(pitch);
        let mut result = self.diatonic_steps_between(pertinent_root, pitch);
        result.degree += 1;
        result
    }

    fn find_closest_root_beneath(&self, pitch: MidiByte) -> MidiByte {
        let mut pertinent_root = self.root();
        while pertinent_root + NOTES_PER_OCTAVE <= pitch {
            pertinent_root += NOTES_PER_OCTAVE;
        }
        pertinent_root
    }

    fn num_sharps(&self) -> usize {
        sharps_for(self.root() as usize, self.root_pos.a())
    }

    fn num_flats(&self) -> usize {
        flats_for(self.root() as usize, self.root_pos.a())
    }

    pub fn is_sharp_key(&self) -> bool {
        self.num_sharps() < self.num_flats()
    }

    pub fn is_flat_key(&self) -> bool {
        let flats = self.num_flats();
        self.num_sharps() >= flats && flats > 0
    }

    /// If `pitch` belongs to this `MusicMode`, returns whether it is a
    /// flat, sharp, or natural note in the scale. If it does not belong,
    /// returns `None`.
    pub fn accidental_for(&self, pitch: MidiByte) -> Option<Accidental> {
        self.diatonic_note_name(pitch).map(|(_, acc)| acc)
    }

    pub fn diatonic_note_name(&self, pitch: MidiByte) -> Option<(NoteLetter, Accidental)> {
        self.diatonic_degree(pitch)
            .pure_degree()
            .map(|degree| self.note_names()[(degree - 1) as usize])
    }

    pub fn note_name(&self, pitch: MidiByte) -> (NoteLetter, Accidental) {
        self.diatonic_note_name(pitch).unwrap_or_else(|| {
            let (_, name, acc) = self.closest_scale_match(pitch);
            (name, acc)
        })
    }

    pub fn note_str(&self, pitch: MidiByte) -> String {
        let (name, acc) = self.note_name(pitch);
        Self::format_note(name, acc)
    }

    fn closest_scale_match(&self, pitch: MidiByte) -> (MidiByte, NoteLetter, Accidental) {
        let mut diatonic_pitch = self.closest_pitch_below(pitch);
        let (mut diatonic_name, diatonic_accidental) =
            self.diatonic_note_name(diatonic_pitch).unwrap();
        let pitch_accidental = match diatonic_accidental {
            Accidental::Natural => Accidental::Sharp,
            Accidental::Flat => Accidental::Natural,
            Accidental::Sharp => {
                diatonic_pitch = self.closest_pitch_above(pitch);
                let diatonic = self.diatonic_note_name(diatonic_pitch).unwrap();
                diatonic_name = diatonic.0;
                match diatonic.1 {
                    Accidental::Sharp => Accidental::Natural,
                    _ => panic!("We are in a sharp key! This can't happen"),
                }
            }
        };
        (diatonic_pitch, diatonic_name, pitch_accidental)
    }

    /// Returns 0 for the Middle C/C#/Cb position.
    /// Returns positive numbers for the treble clef.
    /// Returns negative numbers for the bass clef.
    pub fn staff_position(&self, pitch: MidiByte) -> (MidiByte, Option<Accidental>) {
        let (pitch, acc) = if self.contains(pitch) {
            (pitch, None)
        } else {
            let closest = self.closest_scale_match(pitch);
            (closest.0, Some(closest.2))
        };
        (
            self.diatonic_steps_between(self.c_value(), pitch)
                .pure_degree()
                .unwrap(),
            acc,
        )
    }

    pub fn key_signature(&self) -> KeySignature {
        if self.is_sharp_key() {
            KeySignature {
                notes: Self::traverse_fifths(SHARP_START, |note| *note += 4, self.num_sharps()),
                accidental: Accidental::Sharp,
            }
        } else if self.is_flat_key() {
            KeySignature {
                notes: Self::traverse_fifths(FLAT_START, |note| *note -= 4, self.num_flats()),
                accidental: Accidental::Flat,
            }
        } else {
            KeySignature {
                notes: vec![],
                accidental: Accidental::Natural,
            }
        }
    }

    fn traverse_fifths<U: FnMut(&mut ModNumC<usize, DIATONIC_SCALE_SIZE>)>(
        start: usize,
        mut update: U,
        goal: usize,
    ) -> Vec<NoteLetter> {
        let mut notes = vec![];
        let mut note: ModNumC<usize, DIATONIC_SCALE_SIZE> = ModNumC::new(start);
        for _ in 0..goal {
            notes.push(DIATONIC_NOTE_LETTERS[note.a()]);
            update(&mut note);
        }
        notes
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct KeySignature {
    notes: Vec<NoteLetter>,
    accidental: Accidental,
}

const NUM_NOTES_ON_STAFF: usize = 11;
const TREBLE_INITIAL_OFFSET: MidiByte = 3;
const TREBLE_TO_BASS_OFFSET: MidiByte = -14;

impl KeySignature {
    pub fn c_major() -> MusicMode {
        MusicMode::new(ModNumC::new(0), 0)
    }

    pub fn len(&self) -> usize {
        self.notes.len()
    }

    pub fn symbol(&self) -> Accidental {
        self.accidental
    }

    fn constrain_up(staff_position: MidiByte) -> MidiByte {
        OffsetNumC::<MidiByte, 7, 5>::new(staff_position).a()
    }

    fn constrain_staff(staff_position: MidiByte) -> MidiByte {
        OffsetNumC::<MidiByte, NUM_NOTES_ON_STAFF, 1>::new(staff_position).a()
    }

    fn constrain(staff_position: MidiByte, direction: MidiByte) -> MidiByte {
        if direction > 0 {
            Self::constrain_up(staff_position)
        } else {
            Self::constrain_staff(staff_position)
        }
    }

    pub fn treble_clef(&self) -> Vec<MidiByte> {
        let (offset, direction) = match self.accidental {
            Accidental::Sharp => (-TREBLE_INITIAL_OFFSET, 1),
            Accidental::Flat => (TREBLE_INITIAL_OFFSET, -1),
            Accidental::Natural => return vec![],
        };
        let c_major = Self::c_major();
        let middle_c = c_major.c_value();
        let start1 = Self::constrain_up(
            c_major
                .diatonic_steps_between(middle_c, middle_c + self.notes[0].natural_pitch())
                .pure_degree()
                .unwrap(),
        );
        let mut frontier = [start1, Self::constrain_up(start1 + offset)];
        let mut result = vec![];
        for (i, _) in self.notes.iter().enumerate() {
            result.push(frontier[i % 2]);
            frontier[i % 2] = Self::constrain(frontier[i % 2] + direction, direction);
        }
        result
    }

    pub fn bass_clef(&self) -> Vec<MidiByte> {
        self.treble_clef()
            .drain(..)
            .map(|p| p + TREBLE_TO_BASS_OFFSET)
            .collect()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Accidental {
    Flat,
    Natural,
    Sharp,
}

impl Accidental {
    pub fn symbol(&self) -> char {
        match self {
            Accidental::Flat => '\u{266d}',
            Accidental::Natural => '\u{266e}',
            Accidental::Sharp => '\u{266f}',
        }
    }

    pub fn pitch_shift(&self) -> MidiByte {
        match self {
            Accidental::Flat => -1,
            Accidental::Natural => 0,
            Accidental::Sharp => 1,
        }
    }
}

// Inspired by: https://figuringoutmelody.com/the-24-universal-melodic-figures/
#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence, Hash, Ord, PartialOrd)]
pub struct MelodicFigure {
    shape: MelodicFigureShape,
    polarity: FigurePolarity,
    direction: FigureDirection,
}

impl MelodicFigure {
    pub fn pattern(&self) -> Vec<MidiByte> {
        let mut result = self.shape.pattern();
        if self.polarity == FigurePolarity::Negative {
            for n in result.iter_mut() {
                *n = -*n;
            }
        }
        if self.direction == FigureDirection::Reverse {
            result.reverse();
        }
        result
    }

    pub fn len(&self) -> usize {
        self.pattern().len() + 1
    }

    /// Returns the net change of diatonic steps from the start to the end of this `MelodicFigure`.
    pub fn total_diatonic_change(&self) -> MidiByte {
        self.pattern().iter().sum()
    }
    /*
    // Proposed new version
    pub fn total_diatonic_change(&self) -> DiatonicInterval {
        DiatonicInterval::pure(self.pattern().iter().sum())
    }
    */

    /// Two `MelodicFigure` objects `interfere()` if they overlap anywhere except at their endpoints.
    pub fn interfere(
        &self,
        self_start: usize,
        self_len: usize,
        other_start: usize,
        other_len: usize,
    ) -> bool {
        let mut self_range = self_start..(self_start + self_len);
        let other_range = (other_start + 1)..(other_start + other_len - 1);
        self_range.any(|i| other_range.contains(&i))
    }

    pub fn all_match_starts_in(&self, melody: &Melody, scale: &MusicMode) -> Vec<FigureStart> {
        (0..melody.len())
            .filter_map(|i| {
                self.match_length(melody, scale, i)
                    .map(|length| FigureStart { start: i, length })
            })
            .collect()
    }

    pub fn match_length(&self, melody: &Melody, scale: &MusicMode, start: usize) -> Option<usize> {
        let mut p = 0;
        let mut m = start;
        while p < self.pattern().len() && m + 1 < melody.len() {
            if melody[m].pitch != melody[m + 1].pitch {
                match scale
                    .diatonic_steps_between(melody[m].pitch, melody[m + 1].pitch)
                    .pure_degree()
                {
                    None => return None,
                    Some(actual) => {
                        if actual != self.pattern()[p] {
                            return None;
                        }
                    }
                }
                p += 1;
            }
            m += 1;
        }
        if p == self.pattern().len() {
            Some(m - start + 1)
        } else {
            None
        }
    }

    /// Generates a sequence of diatonic pitches derived from `scale` corresponding to
    /// `self.pattern()`. The sequence starts at `start_pitch` (which is included in the result).
    pub fn make_pitches(&self, start_pitch: MidiByte, scale: &MusicMode) -> VecDeque<MidiByte> {
        let mut result = VecDeque::from([start_pitch]);
        for interval in self.pattern() {
            result.push_back(
                scale.next_pitch(*result.back().unwrap(), DiatonicInterval::pure(interval)),
            );
        }
        result
    }

    pub fn interval2figures(figure_length: usize) -> BTreeMap<MidiByte, Vec<Self>> {
        let mut result = BTreeMap::new();
        for m in all::<MelodicFigure>() {
            if m.len() == figure_length {
                let interval = m.total_diatonic_change();
                match result.get_mut(&interval) {
                    None => {
                        result.insert(interval, vec![m]);
                    }
                    Some(v) => {
                        v.push(m);
                    }
                }
            }
        }
        result
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct FigureStart {
    start: usize,
    length: usize,
}

impl FigureStart {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.start + self.length - 1
    }

    pub fn contains(&self, i: usize) -> bool {
        i >= self.start() && i <= self.end()
    }

    pub fn overlap_with(&self, other: &FigureStart) -> FigureStartOverlap {
        if self.start() == other.start() && self.end() == other.end() {
            FigureStartOverlap::Matches
        } else if !self.contains(other.start())
            && !self.contains(other.end())
            && !other.contains(self.start())
            && !other.contains(self.end())
        {
            FigureStartOverlap::Disjoins
        } else if self.contains(other.start()) && self.contains(other.end()) {
            FigureStartOverlap::Contains
        } else if other.contains(self.start()) && other.contains(self.end()) {
            FigureStartOverlap::ContainedBy
        } else {
            FigureStartOverlap::Intersects
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum FigureStartOverlap {
    Contains,
    ContainedBy,
    Matches,
    Intersects,
    Disjoins,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence, Hash, Ord, PartialOrd)]
pub enum FigurePolarity {
    Positive,
    Negative,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence, Hash, Ord, PartialOrd)]
pub enum FigureDirection {
    Forward,
    Reverse,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence, Hash, Ord, PartialOrd)]
pub enum MelodicFigureShape {
    Note3Scale,
    Auxiliary,
    Arpeggio,
    Run,
    Trill1,
    Trill2,
    Arch,
    NP3,
    PivotLHP,
    ReturnCrazyDriver,
    ArpeggioPlus,
    Parkour1,
    ParkourBounce2,
    ParkourPounce2,
    Vault4,
    Vault5,
    Vault6,
    Vault7,
    Roll,
    DoubleNeighbor,
    Double3rd,
    Pendulum43,
    Pendulum54,
    LeapingScale,
    LeapingAux1,
    LeapingAux2,
    PendulumAux1,
    PendulumAux2,
    Funnel,
    Cambiata1,
    Cambiata2,
    ZigZag1,
    ZigZag2,
}

impl MelodicFigureShape {
    pub fn pattern(&self) -> Vec<MidiByte> {
        match self {
            MelodicFigureShape::Note3Scale => vec![1, 1],
            MelodicFigureShape::Auxiliary => vec![-1, 1],
            MelodicFigureShape::Arpeggio => vec![2, 2],
            MelodicFigureShape::Run => vec![1, 1, 1],
            MelodicFigureShape::Trill1 => vec![1, -1, 1],
            MelodicFigureShape::Trill2 => vec![2, -2, 2],
            MelodicFigureShape::Arch => vec![2, 2, -2],
            MelodicFigureShape::NP3 => vec![-2, -1],
            MelodicFigureShape::PivotLHP => vec![1, -2],
            MelodicFigureShape::ReturnCrazyDriver => vec![1, 1, -1],
            MelodicFigureShape::ArpeggioPlus => vec![2, 2, -1],
            MelodicFigureShape::Parkour1 => vec![-1, 3],
            MelodicFigureShape::ParkourPounce2 => vec![1, -6],
            MelodicFigureShape::ParkourBounce2 => vec![-5, 1],
            MelodicFigureShape::Vault4 => vec![4, 1],
            MelodicFigureShape::Vault5 => vec![5, 1],
            MelodicFigureShape::Vault6 => vec![6, 1],
            MelodicFigureShape::Vault7 => vec![1, 7],
            MelodicFigureShape::Roll => vec![1, 1, -2],
            MelodicFigureShape::DoubleNeighbor => vec![1, -2, 1],
            MelodicFigureShape::Double3rd => vec![2, -1, 2],
            MelodicFigureShape::Pendulum43 => vec![4, -3],
            MelodicFigureShape::Pendulum54 => vec![5, -4],
            MelodicFigureShape::LeapingScale => vec![1, 1, 2],
            MelodicFigureShape::LeapingAux1 => vec![-1, 1, 4],
            MelodicFigureShape::LeapingAux2 => vec![1, -1, 4],
            MelodicFigureShape::PendulumAux1 => vec![4, -5, 1],
            MelodicFigureShape::PendulumAux2 => vec![4, -3, -1],
            MelodicFigureShape::Funnel => vec![3, -2, 1],
            MelodicFigureShape::Cambiata1 => vec![1, 2, -1],
            MelodicFigureShape::Cambiata2 => vec![1, -4, 5],
            MelodicFigureShape::ZigZag1 => vec![4, -2, 5],
            MelodicFigureShape::ZigZag2 => vec![-1, 5, -1],
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::{
        flats_for, major_flats_for, major_sharps_for, sharps_for, Accidental, DiatonicInterval,
        FigureDirection, FigurePolarity, MelodicFigure, MelodicFigureShape, Melody,
        MelodyDirection, MelodyMaker, MelodySection, MidiByte, MusicMode, Note, NoteLetter,
        DIATONIC_SCALE_SIZE,
    };
    use bare_metal_modulo::ModNumC;
    use float_cmp::assert_approx_eq;
    use ordered_float::OrderedFloat;
    use std::cmp::{max, min};
    use std::collections::{BTreeSet, VecDeque};

    const EXAMPLE_MELODY: &str = "55,0.39,0.91,55,0.04,0.0,59,0.33,0.73,60,0.06,0.44,62,0.02,0.87,59,0.05,0.0,60,0.16,0.0,62,0.2,0.0,55,0.39,0.61,55,0.01,0.0,57,0.34,0.98,57,0.05,0.0,55,0.39,0.78,54,0.02,0.98,55,0.19,0.0,54,0.12,0.0,52,0.11,0.74,52,0.0,0.0,54,0.12,0.46,54,0.03,0.0,50,0.1,0.84,50,0.27,0.0,55,0.27,0.74,55,0.1,0.0,59,0.27,0.44,60,0.07,0.54,62,0.04,0.91,59,0.09,0.0,60,0.11,0.0,62,0.19,0.0,55,0.29,0.67,55,0.07,0.0,57,0.32,0.76,57,0.06,0.0,55,0.23,0.7,55,0.05,0.0,54,0.12,0.93,54,0.07,0.0,50,0.37,0.8,50,0.5,0.0,55,0.36,0.76,55,0.05,0.0,59,0.28,0.76,60,0.05,0.7,62,0.01,0.91,59,0.07,0.0,60,0.15,0.0,62,0.2,0.0,55,0.33,0.67,55,0.02,0.0,57,0.29,0.8,57,0.1,0.0,55,0.29,0.9,55,0.08,0.0,54,0.16,1.0,54,0.12,0.0,52,0.12,0.72,54,0.01,0.71,52,0.14,0.0,54,0.07,0.0,50,0.1,0.76,50,0.23,0.0,55,0.22,0.65,55,0.13,0.0,57,0.29,0.64,57,0.08,0.0,55,0.23,0.76,55,0.07,0.0,54,0.12,0.99,54,0.04,0.0,52,0.24,0.95,52,0.19,0.0,54,0.13,1.0,54,0.15,0.0,52,0.12,0.72,52,0.03,0.0,54,0.19,0.83,54,0.13,0.0,50,0.06,0.69,50,0.15,0.0,55,0.01,0.73,57,0.07,0.66,57,0.55,0.0,55,1.5,0.0";
    const COUNTDOWN_MELODY: &str = "66,0.42,1.0,66,0.55,0.0,73,0.17,1.0,73,0.01,0.0,71,0.13,0.77,71,0.0,0.0,73,0.45,0.41,73,0.13,0.0,66,0.85,0.8,66,0.32,0.0,74,0.16,1.0,74,0.0,0.0,74,0.37,0.87,74,0.03,0.0,73,0.2,1.0,73,0.03,0.0,71,0.03,0.06,71,0.04,0.0,71,0.93,1.0,71,0.27,0.0,74,0.16,1.0,74,0.03,0.0,73,0.13,1.0,73,0.03,0.0,74,0.45,1.0,74,0.12,0.0,66,0.58,0.8,66,0.5,0.0,71,0.15,0.75,71,0.02,0.0,71,0.13,0.81,71,0.03,0.0,71,0.21,1.0,71,0.08,0.0,69,0.24,0.94,69,0.08,0.0,68,0.22,0.65,68,0.07,0.0,71,0.24,1.0,71,0.06,0.0,69,0.68,1.0,69,0.15,0.0,73,0.16,1.0,73,0.03,0.0,71,0.14,0.91,71,0.03,0.0,73,0.29,1.0,73,0.22,0.0,66,0.61,0.64,66,0.45,0.0,74,0.15,0.87,74,0.04,0.0,74,0.14,0.83,74,0.02,0.0,74,0.2,1.0,74,0.13,0.0,73,0.29,0.96,73,0.0,0.0,72,0.04,0.49,72,0.03,0.0,71,1.01,1.0,71,0.41,0.0,74,0.14,0.94,74,0.04,0.0,73,0.13,0.8,73,0.03,0.0,74,0.49,1.0,74,0.12,0.0,66,0.93,0.54,66,0.19,0.0,71,0.16,0.81,71,0.02,0.0,71,0.13,0.79,71,0.03,0.0,71,0.21,0.87,71,0.11,0.0,69,0.24,0.86,69,0.08,0.0,68,0.24,0.67,68,0.07,0.0,71,0.24,1.0,71,0.11,0.0,69,0.75,0.86,69,0.05,0.0,68,0.18,0.71,68,0.02,0.0,69,0.16,0.89,69,0.04,0.0,71,0.02,0.99,71,0.0,0.0,83,0.01,1.0,83,0.0,0.0,71,0.56,0.98,71,0.16,0.0,69,0.19,1.0,69,0.04,0.0,71,0.2,1.0,71,0.05,0.0,73,0.24,1.0,73,0.0,0.0,72,0.03,0.62,72,0.07,0.0,71,0.2,0.91,71,0.03,0.0,69,0.01,0.06,69,0.06,0.0,69,0.18,0.73,69,0.11,0.0,68,0.19,0.46,68,0.18,0.0,66,0.51,0.76,66,0.17,0.0,74,0.56,1.0,74,0.01,0.0,73,1.09,0.79,73,0.07,0.0,75,0.16,0.9,75,0.03,0.0,73,0.16,0.84,73,0.03,0.0,71,0.18,0.57,71,0.03,0.0,73,0.78,0.64,73,0.06,0.0,73,0.14,0.91,73,0.04,0.0,73,0.14,0.87,73,0.04,0.0,73,0.26,0.81,73,0.1,0.0,71,0.23,0.91,71,0.07,0.0,69,0.19,0.98,69,0.1,0.0,68,0.23,0.59,68,0.15,0.0,66,1.22,0.68,66,2.0,0.0";
    const COUNTDOWN_ECHO: &str = "[[66, 0.42, 1.0], [66, 0.55, 0.0], [73, 0.17, 1.0], [73, 0.01, 0.0], [71, 0.13, 0.76], [71, 0.0, 0.0], [73, 0.45, 0.40], [73, 0.13, 0.0], [66, 0.85, 0.79], [66, 0.32, 0.0], [74, 0.16, 1.0], [74, 0.0, 0.0], [74, 0.37, 0.86], [74, 0.03, 0.0], [73, 0.2, 1.0], [73, 0.03, 0.0], [71, 0.03, 0.05], [71, 0.04, 0.0], [71, 0.93, 1.0], [71, 0.27, 0.0], [74, 0.16, 1.0], [74, 0.03, 0.0], [73, 0.13, 1.0], [73, 0.03, 0.0], [74, 0.45, 1.0], [74, 0.12, 0.0], [66, 0.58, 0.79], [66, 0.5, 0.0], [71, 0.15, 0.74], [71, 0.02, 0.0], [71, 0.13, 0.80], [71, 0.03, 0.0], [71, 0.21, 1.0], [71, 0.08, 0.0], [69, 0.24, 0.93], [69, 0.08, 0.0], [68, 0.22, 0.64], [68, 0.07, 0.0], [71, 0.24, 1.0], [71, 0.06, 0.0], [69, 0.68, 1.0], [69, 0.15, 0.0], [73, 0.16, 1.0], [73, 0.03, 0.0], [71, 0.14, 0.90], [71, 0.03, 0.0], [73, 0.29, 1.0], [73, 0.22, 0.0], [66, 0.61, 0.63], [66, 0.45, 0.0], [74, 0.15, 0.86], [74, 0.04, 0.0], [74, 0.14, 0.82], [74, 0.02, 0.0], [74, 0.2, 1.0], [74, 0.13, 0.0], [73, 0.29, 0.95], [73, 0.0, 0.0], [72, 0.04, 0.48], [72, 0.03, 0.0], [71, 1.01, 1.0], [71, 0.41, 0.0], [74, 0.14, 0.93], [74, 0.04, 0.0], [73, 0.13, 0.79], [73, 0.03, 0.0], [74, 0.49, 1.0], [74, 0.12, 0.0], [66, 0.93, 0.53], [66, 0.19, 0.0], [71, 0.16, 0.80], [71, 0.02, 0.0], [71, 0.13, 0.78], [71, 0.03, 0.0], [71, 0.21, 0.86], [71, 0.11, 0.0], [69, 0.24, 0.85], [69, 0.08, 0.0], [68, 0.24, 0.66], [68, 0.07, 0.0], [71, 0.24, 1.0], [71, 0.11, 0.0], [69, 0.75, 0.85], [69, 0.05, 0.0], [68, 0.18, 0.70], [68, 0.02, 0.0], [69, 0.16, 0.88], [69, 0.04, 0.0], [71, 0.02, 0.98], [71, 0.0, 0.0], [83, 0.01, 1.0], [83, 0.0, 0.0], [71, 0.56, 0.97], [71, 0.16, 0.0], [69, 0.19, 1.0], [69, 0.04, 0.0], [71, 0.2, 1.0], [71, 0.05, 0.0], [73, 0.24, 1.0], [73, 0.0, 0.0], [72, 0.03, 0.61], [72, 0.07, 0.0], [71, 0.2, 0.90], [71, 0.03, 0.0], [69, 0.01, 0.05], [69, 0.06, 0.0], [69, 0.18, 0.72], [69, 0.11, 0.0], [68, 0.19, 0.45], [68, 0.18, 0.0], [66, 0.51, 0.75], [66, 0.17, 0.0], [74, 0.56, 1.0], [74, 0.01, 0.0], [73, 1.09, 0.78], [73, 0.07, 0.0], [75, 0.16, 0.89], [75, 0.03, 0.0], [73, 0.16, 0.83], [73, 0.03, 0.0], [71, 0.18, 0.56], [71, 0.03, 0.0], [73, 0.78, 0.63], [73, 0.06, 0.0], [73, 0.14, 0.90], [73, 0.04, 0.0], [73, 0.14, 0.86], [73, 0.04, 0.0], [73, 0.26, 0.80], [73, 0.1, 0.0], [71, 0.23, 0.90], [71, 0.07, 0.0], [69, 0.19, 0.97], [69, 0.1, 0.0], [68, 0.23, 0.58], [68, 0.15, 0.0], [66, 1.22, 0.67], [66, 2.0, 0.0]]";
    const NUM_RANDOM_TESTS: usize = 20;

    #[test]
    fn test_parse_melody() {
        let m = "69,0.24,1.0,69,0.09,0.0,72,0.31,1.0,72,0.08,0.0,71,0.29,0.69";
        let notes = Melody::from_str(m);
        println!("{}", notes.view_notes());
        assert_eq!(format!("{:?}", notes), "Melody { notes: [Note { pitch: 69, duration: OrderedFloat(0.24), velocity: 127 }, Note { pitch: 69, duration: OrderedFloat(0.09), velocity: 0 }, Note { pitch: 72, duration: OrderedFloat(0.31), velocity: 127 }, Note { pitch: 72, duration: OrderedFloat(0.08), velocity: 0 }, Note { pitch: 71, duration: OrderedFloat(0.29), velocity: 87 }] }");
    }

    #[test]
    fn test_distinct_consecutive_pitches_in() {
        let melody = lean_on_me_melody();
        let pitches: Vec<MidiByte> = melody.iter().map(|n| n.pitch()).collect();
        println!("{pitches:?}");
        for (start, end, count) in [
            (0, 3, 1),
            (0, 4, 2),
            (0, 8, 4),
            (0, 13, 4),
            (0, 19, 9),
            (0, 22, 9),
            (22, 25, 3),
            (22, 32, 6),
        ] {
            assert_eq!(count, melody.distinct_consecutive_pitches_in(start..=end));
        }
    }

    #[test]
    fn test_randomized_subsection() {
        let maker = MelodyMaker::new();
        let melody = lean_on_me_melody();
        for _ in 0..2 {
            let mut variant = melody.clone();
            let start = 0;
            let end = variant.len() - 1;
            maker.randomize_subsection(&mut variant, start..=end);
            assert_eq!(variant.len(), melody.len());
            assert_eq!(variant[0], melody[0]);
            assert_eq!(
                variant[variant.len() - 1].pitch() % 12,
                melody[variant.len() - 1].pitch() % 12
            );
        }
    }

    fn test_natural_mode(root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>, notes: [MidiByte; 15]) {
        let mode = MusicMode::new(root_pos, notes[0]);
        println!("{} {:?}", mode.name(), mode);
        for (i, n) in notes.iter().enumerate() {
            let i = i as MidiByte;
            let next = mode.next_pitch(notes[0], DiatonicInterval::pure(i));
            assert_eq!(next, *n);
            assert!(mode.contains(*n));
            let prev = mode.next_pitch(*n, -DiatonicInterval::pure(i));
            assert_eq!(notes[0], prev);
            assert!(mode.contains(prev));
        }

        let mut prev = notes[0];
        for n in notes.iter().skip(1) {
            let next = mode.next_pitch(prev, DiatonicInterval::pure(1));
            assert_eq!(next, *n);
            prev = next;
        }

        let not_c_notes: [MidiByte; 5] = [73, 75, 78, 80, 82];
        for n in not_c_notes.iter() {
            assert!(!mode.contains(*n));
        }
    }

    #[test]
    fn test_scales() {
        test_natural_mode(
            ModNumC::new(0),
            [72, 74, 76, 77, 79, 81, 83, 84, 86, 88, 89, 91, 93, 95, 96],
        );
    }

    #[test]
    fn test_scales_dorian() {
        test_natural_mode(
            ModNumC::new(1),
            [74, 76, 77, 79, 81, 83, 84, 86, 88, 89, 91, 93, 95, 96, 98],
        );
    }

    #[test]
    fn test_modes() {
        let modes = MusicMode::all_modes_for(72);
        let expected: [[MidiByte; DIATONIC_SCALE_SIZE]; DIATONIC_SCALE_SIZE] = [
            [0, 2, 4, 5, 7, 9, 11],
            [10, 0, 2, 3, 5, 7, 9],
            [8, 10, 0, 1, 3, 5, 7],
            [7, 9, 11, 0, 2, 4, 6],
            [5, 7, 9, 10, 0, 2, 4],
            [3, 5, 7, 8, 10, 0, 2],
            [1, 3, 5, 6, 8, 10, 0],
        ];
        for i in 0..DIATONIC_SCALE_SIZE {
            assert_eq!(modes[i].octave_notes, expected[i]);
        }
    }

    #[test]
    fn show_interval_table() {
        println!("Length 3 figures");
        let table = MelodicFigure::interval2figures(3);
        for (i, ms) in table.iter() {
            println!("{}: {:?}", *i, ms);
        }

        println!("Length 4 figures");
        let table = MelodicFigure::interval2figures(4);
        for (i, ms) in table.iter() {
            println!("{}: {:?}", *i, ms);
        }
    }

    #[test]
    fn test_diatonic_bug() {
        let mode = MusicMode {
            root_pos: ModNumC::new(0),
            octave_notes: [
                ModNumC::new(7),
                ModNumC::new(9),
                ModNumC::new(11),
                ModNumC::new(0),
                ModNumC::new(2),
                ModNumC::new(4),
                ModNumC::new(6),
            ],
            intervals: [2, 2, 1, 2, 2, 2, 1],
        };
        let steps = mode.diatonic_steps_between(54, 57);
        assert_eq!(steps, DiatonicInterval::pure(2));
    }

    #[test]
    fn test_next_note_bug() {
        let mode = MusicMode {
            root_pos: ModNumC::new(0),
            octave_notes: [
                ModNumC::new(7),
                ModNumC::new(9),
                ModNumC::new(11),
                ModNumC::new(0),
                ModNumC::new(2),
                ModNumC::new(4),
                ModNumC::new(6),
            ],
            intervals: [2, 2, 1, 2, 2, 2, 1],
        };
        println!("mode: {}", mode.name());
        println!("{:?}", mode);
        let tests = [
            (54, 1, 55),
            (67, 1, 69),
            (69, 1, 71),
            (71, 1, 72),
            (60, 1, 62),
            (62, 1, 64),
            (64, 1, 66),
            (66, 1, 67),
            (79, 1, 81),
            (81, 1, 83),
            (83, 1, 84),
            (72, 1, 74),
            (74, 1, 76),
            (76, 1, 78),
            (78, 1, 79),
            (91, 1, 93),
            (93, 1, 95),
            (95, 1, 96),
            (84, 1, 86),
            (86, 1, 88),
            (88, 1, 90),
            (90, 1, 91),
            (103, 1, 105),
            (105, 1, 107),
            (107, 1, 108),
            (96, 1, 98),
            (98, 1, 100),
            (100, 1, 102),
            (102, 1, 103),
            (115, 1, 117),
            (117, 1, 119),
            (108, 1, 110),
            (110, 1, 112),
            (112, 1, 114),
            (114, 1, 115),
            (120, 1, 122),
            (122, 1, 124),
        ];
        for (reference_pitch, scale_steps_away, expected) in tests {
            let np = mode.next_pitch(reference_pitch, DiatonicInterval::pure(scale_steps_away));
            assert_eq!(np, expected);
        }
    }

    #[test]
    fn test_send_back() {
        let tune = Melody::from_str(COUNTDOWN_MELODY);
        assert_eq!(tune.sonic_pi_list(), COUNTDOWN_ECHO);
    }

    #[test]
    fn test_diatonic_degree() {
        let melody = Melody::from_str(EXAMPLE_MELODY);
        let scale = melody.best_scale_for();
        assert_eq!(scale.name(), "G Ionian");
        for (i, pitch) in [67, 69, 71, 72, 74, 76, 78, 79, 81, 83, 84, 86, 88, 90, 91]
            .iter()
            .enumerate()
        {
            assert_eq!(
                DiatonicInterval::pure((i % DIATONIC_SCALE_SIZE) as MidiByte + 1),
                scale.diatonic_degree(*pitch)
            );
        }
    }

    #[test]
    fn test_figure_match_1() {
        let (figure_count, figure_set) = test_figure_match(EXAMPLE_MELODY);
        assert_eq!(figure_count, 63);
        assert_eq!(figure_set.len(), 16);
        assert_eq!(format!("{:?}", figure_set), "{MelodicFigure { shape: Note3Scale, polarity: Positive, direction: Forward }, MelodicFigure { shape: Auxiliary, polarity: Positive, direction: Forward }, MelodicFigure { shape: Auxiliary, polarity: Positive, direction: Reverse }, MelodicFigure { shape: Run, polarity: Negative, direction: Forward }, MelodicFigure { shape: Trill1, polarity: Positive, direction: Forward }, MelodicFigure { shape: Trill1, polarity: Negative, direction: Forward }, MelodicFigure { shape: NP3, polarity: Positive, direction: Reverse }, MelodicFigure { shape: ReturnCrazyDriver, polarity: Negative, direction: Forward }, MelodicFigure { shape: ReturnCrazyDriver, polarity: Negative, direction: Reverse }, MelodicFigure { shape: Roll, polarity: Positive, direction: Forward }, MelodicFigure { shape: Roll, polarity: Positive, direction: Reverse }, MelodicFigure { shape: DoubleNeighbor, polarity: Positive, direction: Forward }, MelodicFigure { shape: LeapingScale, polarity: Positive, direction: Reverse }, MelodicFigure { shape: LeapingScale, polarity: Negative, direction: Forward }, MelodicFigure { shape: LeapingAux2, polarity: Negative, direction: Reverse }, MelodicFigure { shape: Funnel, polarity: Positive, direction: Reverse }}");
    }

    #[test]
    fn test_figure_match_countdown() {
        let (figure_count, figure_set) = test_figure_match(COUNTDOWN_MELODY);
        assert_eq!(figure_count, 106);
        assert_eq!(figure_set.len(), 23);
        assert_eq!(format!("{:?}", figure_set), "{MelodicFigure { shape: Note3Scale, polarity: Positive, direction: Forward }, MelodicFigure { shape: Note3Scale, polarity: Negative, direction: Forward }, MelodicFigure { shape: Auxiliary, polarity: Positive, direction: Forward }, MelodicFigure { shape: Run, polarity: Negative, direction: Forward }, MelodicFigure { shape: Trill1, polarity: Negative, direction: Forward }, MelodicFigure { shape: PivotLHP, polarity: Negative, direction: Reverse }, MelodicFigure { shape: ReturnCrazyDriver, polarity: Positive, direction: Reverse }, MelodicFigure { shape: ReturnCrazyDriver, polarity: Negative, direction: Forward }, MelodicFigure { shape: ReturnCrazyDriver, polarity: Negative, direction: Reverse }, MelodicFigure { shape: Parkour1, polarity: Positive, direction: Reverse }, MelodicFigure { shape: ParkourBounce2, polarity: Positive, direction: Reverse }, MelodicFigure { shape: ParkourBounce2, polarity: Negative, direction: Forward }, MelodicFigure { shape: Vault7, polarity: Positive, direction: Forward }, MelodicFigure { shape: Vault7, polarity: Negative, direction: Reverse }, MelodicFigure { shape: Roll, polarity: Negative, direction: Forward }, MelodicFigure { shape: Roll, polarity: Negative, direction: Reverse }, MelodicFigure { shape: DoubleNeighbor, polarity: Negative, direction: Forward }, MelodicFigure { shape: Double3rd, polarity: Positive, direction: Forward }, MelodicFigure { shape: LeapingAux2, polarity: Positive, direction: Reverse }, MelodicFigure { shape: LeapingAux2, polarity: Negative, direction: Forward }, MelodicFigure { shape: PendulumAux1, polarity: Negative, direction: Forward }, MelodicFigure { shape: Cambiata2, polarity: Positive, direction: Forward }, MelodicFigure { shape: ZigZag2, polarity: Positive, direction: Forward }}");
    }

    fn test_figure_match(melody_str: &str) -> (usize, BTreeSet<MelodicFigure>) {
        let melody = Melody::from_str(melody_str);
        let scale = melody.best_scale_for();
        println!("scale: {}", scale.name());
        let maker = MelodyMaker::new();
        let figures = maker.all_figure_matches(&melody);
        let figure_set: BTreeSet<MelodicFigure> = figures.iter().map(|(_, f, _)| *f).collect();

        println!("# figures: {}", figures.len());
        println!("# distinct figures: {}", figure_set.len());
        println!("figures: {:?}", figure_set);
        println!("pause indices: {:?}", melody.find_pause_indices());
        (figures.len(), figure_set)
    }

    fn test_variation_unchanged<V: Fn(&MelodyMaker, &Melody, f64) -> Melody>(v_func: V) {
        let melody = Melody::from_str(COUNTDOWN_MELODY);
        let mut maker = MelodyMaker::new();
        let v = v_func(&mut maker, &melody, 0.0);
        assert_eq!(v.len(), melody.len());
        assert_eq!(v.sonic_pi_list().as_str(), COUNTDOWN_ECHO);
    }

    fn test_variation_changed<V: Fn(&MelodyMaker, &Melody, f64) -> Melody>(
        v_func: V,
        expected_lo: f64,
        expected_hi: f64,
    ) {
        let melody = Melody::from_str(COUNTDOWN_MELODY);
        let scale = melody.best_scale_for();
        let mut maker = MelodyMaker::new();
        let mut lo = OrderedFloat(1.0);
        let mut hi = OrderedFloat(0.0);
        for _ in 0..NUM_RANDOM_TESTS {
            let var = v_func(&mut maker, &melody, 1.0);
            assert_eq!(var.len(), melody.len());

            let mut different_count = 0;
            for i in 0..var.len() {
                assert_eq!(var[i].duration, melody[i].duration);
                assert_eq!(var[i].velocity, melody[i].velocity);
                assert!(!scale.contains(melody[i].pitch) || scale.contains(var[i].pitch));
                if var[i].pitch != melody[i].pitch {
                    different_count += 1;
                }
            }
            let portion_changed = different_count as f64 / var.len() as f64;
            lo = min(lo, OrderedFloat(portion_changed));
            hi = max(hi, OrderedFloat(portion_changed));
        }
        println!("lo: {:.2} hi: {:.2}", lo.into_inner(), hi.into_inner());
        assert!(lo > OrderedFloat(expected_lo));
        assert!(hi < OrderedFloat(expected_hi));
    }

    #[test]
    fn test_motive_variation() {
        test_variation_unchanged(MelodyMaker::create_motive_variation);
        test_variation_changed(MelodyMaker::create_motive_variation, 0.10, 0.60);
    }

    #[test]
    fn test_motive_remelodize_unchanged() {
        let melody = Melody::from_str(COUNTDOWN_MELODY);
        melody.tuple_print();
        println!("{melody:?}");
        let maker = MelodyMaker::new();
        let sections = maker.get_melody_sections(&melody);
        let mut variation = melody.clone();
        for section in sections.iter() {
            println!("{section:?}");
            section.remelodize(&mut variation);
            assert_eq!(melody.len(), variation.len());
            for i in 0..melody.len() {
                if melody.notes[i] != variation.notes[i] {
                    let pre_m = melody.notes[..i + 3]
                        .iter()
                        .map(|n| n.pitch())
                        .collect::<Vec<_>>();
                    let pre_v = variation.notes[..i + 3]
                        .iter()
                        .map(|n| n.pitch())
                        .collect::<Vec<_>>();
                    println!("{pre_m:?}");
                    println!("{pre_v:?}");
                    panic!(
                        "Died at {i}: {:?} vs {:?}",
                        melody.notes[i], variation.notes[i]
                    );
                }
            }
            assert_eq!(variation, melody);
        }
    }

    #[test]
    fn remelodize_countdown_bug() {
        let melody = Melody::from_vec(&vec![
            Note::new(74, 0.56, 127),
            Note::new(74, 0.01, 0),
            Note::new(73, 1.09, 100),
            Note::new(73, 0.07, 0),
            Note::new(75, 0.16, 114),
            Note::new(75, 0.03, 0),
            Note::new(73, 0.16, 106),
            Note::new(73, 0.03, 0),
            Note::new(71, 0.18, 72),
            Note::new(71, 0.03, 0),
            Note::new(73, 0.78, 81),
            Note::new(73, 0.06, 0),
            Note::new(73, 0.14, 115),
            Note::new(73, 0.04, 0),
            Note::new(73, 0.14, 110),
            Note::new(73, 0.04, 0),
            Note::new(73, 0.26, 102),
            Note::new(73, 0.1, 0),
            Note::new(71, 0.23, 115),
            Note::new(71, 0.07, 0),
            Note::new(69, 0.19, 124),
            Note::new(69, 0.1, 0),
            Note::new(68, 0.23, 74),
            Note::new(68, 0.15, 0),
            Note::new(66, 1.22, 86),
            Note::new(66, 2.0, 0),
        ]);
        let scale = melody.best_scale_for();
        println!("{} {}", scale.name(), scale.root());
        melody.tuple_print();
        let consolidated = melody.get_consolidated_notes();
        let consolidated_melody = Melody::from_vec(&consolidated.iter().map(|(_, n)| *n).collect());
        println!("{consolidated_melody:?}");
        let intervals = consolidated_melody.diatonic_intervals();
        println!("{intervals:?}");
        let maker = MelodyMaker::new();
        let sections = maker.get_melody_sections(&melody);
        let mut variation = melody.clone();
        for section in sections.iter() {
            println!("{section:?}");
            section.remelodize(&mut variation);
            assert_eq!(melody.len(), variation.len());
            for i in 0..melody.len() {
                if melody.notes[i] != variation.notes[i] {
                    let pre_m = melody.notes[..i + 3]
                        .iter()
                        .map(|n| n.pitch())
                        .collect::<Vec<_>>();
                    let pre_v = variation.notes[..i + 3]
                        .iter()
                        .map(|n| n.pitch())
                        .collect::<Vec<_>>();
                    println!("{pre_m:?}");
                    println!("{pre_v:?}");
                    panic!(
                        "Died at {i}: {:?} vs {:?}",
                        melody.notes[i], variation.notes[i]
                    );
                }
            }
            assert_eq!(variation, melody);
        }
    }

    #[test]
    fn chromatic_bug_1() {
        let scale = MusicMode::new(ModNumC::new(5), 6);
        assert_eq!(scale.name(), "F♯ Aeolian");
        for (p, d, c) in [
            (73, 0, 0),
            (74, 1, 0),
            (75, 1, 1),
            (76, 2, 0),
            (77, 2, 1),
            (78, 3, 0),
        ] {
            assert_eq!(
                scale.diatonic_steps_between(73, p),
                DiatonicInterval::chromatic(d, c)
            );
        }
    }

    #[test]
    fn test_matching_figure() {
        let melody = Melody::from_str(COUNTDOWN_MELODY);
        let scale = melody.best_scale_for();

        let pitch1 = melody.pitch_subsequence_at(0, 4).unwrap();
        assert_eq!(pitch1, vec![66, 73, 71, 73]);
        let gap1 = scale.diatonic_steps_between(*pitch1.first().unwrap(), *pitch1.last().unwrap());
        assert_eq!(gap1, DiatonicInterval::pure(4));
        let figure = MelodicFigure {
            shape: MelodicFigureShape::LeapingAux2,
            polarity: FigurePolarity::Positive,
            direction: FigureDirection::Reverse,
        };
        let pattern = figure.pattern();
        assert_eq!(pattern, vec![4, -1, 1]);
        let m = figure.match_length(&melody, &scale, 0);
        assert_eq!(m.unwrap(), 6);
    }

    #[test]
    fn study_figures() {
        let melody = Melody::from_str(COUNTDOWN_MELODY);
        let scale = melody.best_scale_for();
        let maker = MelodyMaker::new();
        let mut start = 0;
        let mut figures = Vec::new();
        while start < melody.len() {
            if let Some((matched, _matched_len)) = maker.matching_figure(&melody, start) {
                figures.push((
                    Some(matched),
                    matched.make_pitches(melody[start].pitch, &scale),
                ));
            } else {
                figures.push((None, VecDeque::from([melody[start].pitch])));
            }
            start += 1;
        }
        for (i, (figure, notes)) in figures.iter().enumerate() {
            println!("{}: {}", i, melody[i].pitch());
            println!("{:?} {:?}", figure, notes);
            if i > 0 {
                println!(
                    "diatonic jump: {:?}",
                    scale.diatonic_steps_between(
                        *figures[i - 1].1.back().unwrap(),
                        *notes.front().unwrap()
                    )
                );
            }
            println!();
        }
    }

    #[test]
    fn test_synchro() {
        let mut m = Melody::new();
        for (n, d, i) in [
            (57, 0.00, 70),
            (65, 0.01, 100),
            (60, 0.00, 90),
            (64, 0.00, 105),
            (62, 0.15, 100),
            (64, 0.06, 0),
            (57, 0.01, 0),
            (60, 0.03, 0),
            (62, 0.00, 0),
            (65, 0.08, 0),
        ] {
            m.add(Note::new(n, d, i));
        }
        assert!(!m.all_rests_synchronized());
        m.synchronize_rests();
        println!("{m:?}");
        assert!(m.all_rests_synchronized());
    }

    #[test]
    fn test_sharp_flat() {
        for (chrom, sharp, flat) in [
            (0, 0, 0),
            (2, 2, 10),
            (4, 4, 8),
            (5, 11, 1),
            (7, 1, 11),
            (9, 3, 9),
        ]
        .iter()
        {
            assert_eq!(major_sharps_for(*chrom), *sharp);
            assert_eq!(major_flats_for(*chrom), *flat);
        }

        for (d_mode, sharp, flat) in [
            (0, 2, 10),
            (1, 0, 0),
            (2, 10, 2),
            (3, 3, 9),
            (4, 1, 11),
            (5, 11, 1),
            (6, 9, 3),
        ]
        .iter()
        {
            assert_eq!(sharps_for(2, *d_mode), *sharp);
            assert_eq!(flats_for(2, *d_mode), *flat);
        }
    }

    #[test]
    fn test_distinct_seq_len() {
        let countdown = Melody::from_str(COUNTDOWN_MELODY);
        assert_eq!(countdown.distinct_seq_len(0, 3).unwrap(), 6);
        assert_eq!(countdown.distinct_seq_len(8, 3).unwrap(), 8)
    }

    #[test]
    fn test_c_index() {
        for (root, c_value) in [
            (36, 60),
            (37, 60),
            (38, 61),
            (39, 60),
            (40, 61),
            (41, 60),
            (42, 59),
            (43, 60),
            (44, 60),
            (45, 61),
            (46, 60),
            (47, 61),
        ] {
            let scale = MusicMode::new(ModNumC::new(0), root);
            assert_eq!(scale.c_value(), c_value);
        }
    }

    #[test]
    fn test_staff_position() {
        let scale = MusicMode::new(ModNumC::new(2), 65);
        assert_eq!(scale.name(), "F Phrygian");
        assert_eq!(scale.c_value(), 60);
        for (pitch, position, modifier) in [
            (53, -4, None),
            (54, -3, None),
            (55, -3, Some(Accidental::Natural)),
            (56, -2, None),
            (57, -2, Some(Accidental::Natural)),
            (58, -1, None),
            (59, -1, Some(Accidental::Natural)),
            (60, 0, None),
            (61, 1, None),
            (62, 1, Some(Accidental::Natural)),
            (63, 2, None),
            (64, 2, Some(Accidental::Natural)),
            (65, 3, None),
            (66, 4, None),
            (67, 4, Some(Accidental::Natural)),
            (68, 5, None),
            (69, 5, Some(Accidental::Natural)),
            (70, 6, None),
            (71, 6, Some(Accidental::Natural)),
            (72, 7, None),
            (73, 8, None),
            (74, 8, Some(Accidental::Natural)),
            (75, 9, None),
            (76, 9, Some(Accidental::Natural)),
            (77, 10, None),
        ] {
            assert_eq!(scale.staff_position(pitch), (position, modifier));
        }
    }

    #[test]
    fn test_staff_position_2() {
        let scale = MusicMode::new(ModNumC::new(1), 61);
        assert_eq!(scale.name(), "C♯ Dorian");
        assert_eq!(scale.c_value(), 61);
        for (pitch, position, modifier) in [
            (53, -5, Some(Accidental::Sharp)),
            (54, -4, None),
            (55, -3, Some(Accidental::Natural)),
            (56, -3, None),
            (57, -2, Some(Accidental::Natural)),
            (58, -2, None),
            (59, -1, None),
            (60, -1, Some(Accidental::Sharp)),
            (61, 0, None),
            (62, 1, Some(Accidental::Natural)),
            (63, 1, None),
            (64, 2, None),
            (65, 2, Some(Accidental::Sharp)),
            (66, 3, None),
            (67, 4, Some(Accidental::Natural)),
            (68, 4, None),
            (69, 5, Some(Accidental::Natural)),
            (70, 5, None),
            (71, 6, None),
            (72, 6, Some(Accidental::Sharp)),
            (73, 7, None),
            (74, 8, Some(Accidental::Natural)),
            (75, 8, None),
            (76, 9, None),
            (77, 9, Some(Accidental::Sharp)),
        ] {
            assert_eq!(scale.staff_position(pitch), (position, modifier));
        }
    }

    #[test]
    fn test_staff_position_3() {
        let scale = MusicMode::new(ModNumC::new(0), 61);
        assert_eq!(scale.name(), "D♭ Ionian");
        assert_eq!(scale.c_value(), 60);
        for (pitch, position, modifier) in [
            (49, -6, None),
            (50, -6, Some(Accidental::Natural)),
            (51, -5, None),
            (52, -5, Some(Accidental::Natural)),
            (53, -4, None),
            (54, -3, None),
            (55, -3, Some(Accidental::Natural)),
            (56, -2, None),
            (57, -2, Some(Accidental::Natural)),
            (58, -1, None),
            (59, -1, Some(Accidental::Natural)),
            (60, 0, None),
            (61, 1, None),
            (62, 1, Some(Accidental::Natural)),
            (63, 2, None),
            (64, 2, Some(Accidental::Natural)),
            (65, 3, None),
            (66, 4, None),
            (67, 4, Some(Accidental::Natural)),
            (68, 5, None),
            (69, 5, Some(Accidental::Natural)),
            (70, 6, None),
            (71, 6, Some(Accidental::Natural)),
            (72, 7, None),
            (73, 8, None),
        ] {
            assert_eq!(scale.staff_position(pitch), (position, modifier));
        }
    }

    #[test]
    fn test_staff_position_4() {
        let scale = MusicMode::new(ModNumC::new(0), 66);
        assert_eq!(scale.name(), "G♭ Ionian");
        assert_eq!(scale.c_value(), 59);
        for (pitch, position, modifier) in [
            (49, -6, None),
            (50, -6, Some(Accidental::Natural)),
            (51, -5, None),
            (52, -5, Some(Accidental::Natural)),
            (53, -4, None),
            (54, -3, None),
            (55, -3, Some(Accidental::Natural)),
            (56, -2, None),
            (57, -2, Some(Accidental::Natural)),
            (58, -1, None),
            (59, 0, None),
            (60, 0, Some(Accidental::Natural)),
            (61, 1, None),
            (62, 1, Some(Accidental::Natural)),
            (63, 2, None),
            (64, 2, Some(Accidental::Natural)),
            (65, 3, None),
            (66, 4, None),
            (67, 4, Some(Accidental::Natural)),
            (68, 5, None),
            (69, 5, Some(Accidental::Natural)),
            (70, 6, None),
            (71, 7, None),
        ] {
            assert_eq!(scale.staff_position(pitch), (position, modifier));
        }
    }

    #[test]
    fn test_note_names_1() {
        let scale = MusicMode::new(ModNumC::new(0), 66);
        assert_eq!(scale.name(), "G♭ Ionian");
        let goal = [
            (NoteLetter::G, Accidental::Flat),
            (NoteLetter::A, Accidental::Flat),
            (NoteLetter::B, Accidental::Flat),
            (NoteLetter::C, Accidental::Flat),
            (NoteLetter::D, Accidental::Flat),
            (NoteLetter::E, Accidental::Flat),
            (NoteLetter::F, Accidental::Natural),
        ];
        assert_eq!(scale.note_names(), goal);
    }

    #[test]
    fn test_note_names_2() {
        let scale = MusicMode::new(ModNumC::new(1), 61);
        assert_eq!(scale.name(), "C♯ Dorian");
        let goal = [
            (NoteLetter::C, Accidental::Sharp),
            (NoteLetter::D, Accidental::Sharp),
            (NoteLetter::E, Accidental::Natural),
            (NoteLetter::F, Accidental::Sharp),
            (NoteLetter::G, Accidental::Sharp),
            (NoteLetter::A, Accidental::Sharp),
            (NoteLetter::B, Accidental::Natural),
        ];
        assert_eq!(scale.note_names(), goal);
    }

    #[test]
    fn test_key_signature() {
        let scale = MusicMode::new(ModNumC::new(1), 61);
        assert_eq!(scale.name(), "C♯ Dorian");
        assert_eq!(
            format!("{:?}", scale.key_signature()),
            "KeySignature { notes: [F, C, G, D, A], accidental: Sharp }"
        );
    }

    #[test]
    fn test_treble_clef() {
        let scale = MusicMode::new(ModNumC::new(1), 61);
        assert_eq!(scale.name(), "C♯ Dorian");
        assert_eq!(scale.key_signature().treble_clef(), vec![10, 7, 11, 8, 5]);
    }

    #[test]
    fn test_bass_clef() {
        let scale = MusicMode::new(ModNumC::new(1), 61);
        assert_eq!(scale.name(), "C♯ Dorian");
        assert_eq!(scale.key_signature().bass_clef(), vec![-4, -7, -3, -6, -9]);
    }

    #[test]
    fn test_key_signature_2() {
        let scale = MusicMode::new(ModNumC::new(0), 66);
        assert_eq!(scale.name(), "G♭ Ionian");
        assert_eq!(
            format!("{:?}", scale.key_signature()),
            "KeySignature { notes: [B, E, A, D, G, C], accidental: Flat }"
        );
    }

    #[test]
    fn test_treble_clef_2() {
        let scale = MusicMode::new(ModNumC::new(0), 66);
        assert_eq!(scale.name(), "G♭ Ionian");
        assert_eq!(scale.key_signature().treble_clef(), vec![6, 9, 5, 8, 4, 7]);
    }

    #[test]
    fn test_bass_clef_2() {
        let scale = MusicMode::new(ModNumC::new(0), 66);
        assert_eq!(scale.name(), "G♭ Ionian");
        assert_eq!(
            scale.key_signature().bass_clef(),
            vec![-8, -5, -9, -6, -10, -7]
        );
    }

    #[test]
    fn test_key_signature_3() {
        let scale = MusicMode::new(ModNumC::new(0), 60);
        assert_eq!(scale.name(), "C Ionian");
        assert_eq!(
            format!("{:?}", scale.key_signature()),
            "KeySignature { notes: [], accidental: Natural }"
        );
    }

    #[test]
    fn test_key_signature_4() {
        let scale = MusicMode::new(ModNumC::new(5), 64);
        assert_eq!(scale.name(), "E Aeolian");
        assert_eq!(
            format!("{:?}", scale.key_signature()),
            "KeySignature { notes: [F], accidental: Sharp }"
        );
    }

    #[test]
    fn test_key_signature_5() {
        let scale = MusicMode::new(ModNumC::new(5), 62);
        assert_eq!(scale.name(), "D Aeolian");
        assert_eq!(
            format!("{:?}", scale.key_signature()),
            "KeySignature { notes: [B], accidental: Flat }"
        );
    }

    #[test]
    fn test_next_non_diatonic() {
        let root = 60;
        let scale = MusicMode::new(ModNumC::new(0), root);
        assert_eq!(scale.name(), "C Ionian");
        for (i, (degree, chroma)) in [
            (0, 0),
            (0, 1),
            (1, 0),
            (1, 1),
            (2, 0),
            (3, 0),
            (3, 1),
            (4, 0),
            (4, 1),
            (5, 0),
            (5, 1),
            (6, 0),
            (7, 0),
        ]
        .iter()
        .enumerate()
        {
            assert_eq!(
                root + i as MidiByte,
                scale.next_pitch(root, DiatonicInterval::chromatic(*degree, *chroma))
            );
        }
        for (i, (degree, chroma)) in [
            (0, 0),
            (-1, 0),
            (-1, -1),
            (-2, 0),
            (-2, -1),
            (-3, 0),
            (-3, -1),
            (-4, 0),
            (-5, 0),
            (-5, -1),
            (-6, 0),
            (-6, -1),
            (-7, 0),
        ]
        .iter()
        .enumerate()
        {
            assert_eq!(
                root - i as MidiByte,
                scale.next_pitch(root, DiatonicInterval::chromatic(*degree, *chroma))
            );
        }
    }

    #[test]
    fn test_next_chromatic_reference() {
        let scale = MusicMode::new(ModNumC::new(0), 60);
        assert_eq!(scale.name(), "C Ionian");
        for (start, (degree, chroma), expected) in [
            (61, (0, 0), 62),
            (61, (0, 1), 62),
            (61, (1, 0), 64),
            (61, (1, 1), 64),
            (63, (1, -1), 64),
        ] {
            let n = scale.next_pitch(start, DiatonicInterval::chromatic(degree, chroma));
            println!("{n}: {start} ({degree} {chroma}) {expected}");
            assert_eq!(expected, n);
        }
    }

    const LEAN_ON_ME: [(MidiByte, f64, MidiByte); 62] = [
        (60, 0.487445772, 92),
        (60, 0.377421752, 0),
        (60, 0.289316858, 93),
        (60, 0.005971111, 0),
        (62, 0.248933836, 102),
        (62, 0.05767016, 0),
        (64, 0.25962179, 113),
        (64, 0.229479448, 0),
        (65, 0.317320844, 4),
        (65, 0.042830378, 0),
        (65, 0.582655121, 70),
        (65, 0.50379576, 0),
        (65, 0.250825755, 106),
        (65, 0.017210736, 0),
        (64, 0.272029135, 100),
        (64, 0.027428442, 0),
        (62, 1.126041184, 99),
        (64, 0.548192689, 99),
        (62, 0.273066185, 99),
        (60, 0.60045823, 117),
        (60, 0.450277594, 0),
        (60, 0.269494265, 85),
        (60, 0.003552609, 0),
        (62, 0.267746147, 96),
        (62, 0.016828202, 0),
        (64, 0.382390025, 123),
        (64, 0.128571533, 0),
        (64, 0.699718069, 113),
        (64, 0.126759354, 0),
        (62, 0.867493649, 117),
        (62, 0.46433006, 0),
        (64, 0.268483555, 106),
        (60, 1.323782698, 106),
        (60, 0.247095603, 100),
        (60, 0.026361804, 0),
        (62, 0.312539865, 30),
        (62, 0.008570104, 0),
        (64, 0.267542603, 100),
        (64, 0.05672056, 0),
        (65, 0.60457732, 110),
        (65, 0.537155291, 0),
        (65, 0.271879248, 113),
        (65, 0.06866826, 0),
        (64, 0.253815119, 88),
        (64, 0.10896619, 0),
        (65, 1.4822801190000001, 78),
        (64, 0.362781309, 78),
        (62, 0.202632925, 78),
        (64, 0.651313167, 118),
        (64, 0.412422427, 0),
        (64, 0.315570477, 86),
        (64, 0.032453773, 0),
        (57, 1.254315847, 92),
        (65, 0.321109969, 92),
        (64, 0.64096164, 117),
        (64, 0.485079544, 0),
        (55, 0.530547672, 94),
        (55, 0.017645017, 0),
        (62, 0.263664442, 125),
        (62, 0.009401743, 0),
        (60, 0.670999911, 111),
        (60, 1.5000003039999998, 0),
    ];

    #[test]
    fn test_melody_sections() {
        let maker = MelodyMaker::new();
        let melody = lean_on_me_melody();
        let sections = maker.get_melody_sections(&melody);
        println!("{sections:?}");
        assert_eq!(4, sections.len());
        let expected = vec![
            MelodySection {
                intervals: vec![
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: 1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: 1,
                        chroma: 0,
                    },
                ],
                starts: vec![14, 39],
            },
            MelodySection {
                intervals: vec![
                    DiatonicInterval {
                        degree: 1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: 1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: 1,
                        chroma: 0,
                    },
                ],
                starts: vec![0, 32],
            },
            MelodySection {
                intervals: vec![
                    DiatonicInterval {
                        degree: 1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: 1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -2,
                        chroma: 0,
                    },
                ],
                starts: vec![23],
            },
            MelodySection {
                intervals: vec![
                    DiatonicInterval {
                        degree: -4,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: 5,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -5,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: 4,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                ],
                starts: vec![48],
            },
        ];
        assert_eq!(sections, expected);
    }

    #[test]
    fn test_vary() {
        let mut maker = MelodyMaker::new();
        let melody = lean_on_me_melody();
        let scale = melody.best_scale_for();
        let mut num_differ = 0;
        for _ in 0..NUM_RANDOM_TESTS {
            let mut sections = maker.get_melody_sections(&melody);
            for section in sections.iter_mut() {
                println!("before: {section:?}");
                let len_before = section.intervals.len();
                let total_before = section.overall_interval_change(&scale);
                let values_before = section.intervals.clone();
                section.vary(&scale, 1.0, &mut maker);
                println!("after: {section:?}");
                assert_eq!(len_before, section.intervals.len());
                assert_eq!(total_before, section.overall_interval_change(&scale));
                if values_before != section.intervals {
                    num_differ += 1;
                }
            }
        }
        assert!(num_differ > NUM_RANDOM_TESTS);
    }

    #[test]
    fn test_remelodize() {
        let mut melody = lean_on_me_melody();
        let new_sections = vec![
            MelodySection {
                intervals: vec![
                    DiatonicInterval {
                        degree: 1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: 1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                ],
                starts: vec![14, 39],
            },
            MelodySection {
                intervals: vec![
                    DiatonicInterval {
                        degree: 2,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: -1,
                        chroma: 0,
                    },
                    DiatonicInterval {
                        degree: 2,
                        chroma: 0,
                    },
                ],
                starts: vec![0, 32],
            },
        ];
        for section in new_sections {
            section.remelodize(&mut melody);
        }
        let expected = Melody::from_vec(&vec![
            Note::new(60, 0.487445772, 92),
            Note::new(60, 0.377421752, 0),
            Note::new(60, 0.289316858, 93),
            Note::new(60, 0.005971111, 0),
            Note::new(64, 0.248933836, 102),
            Note::new(64, 0.05767016, 0),
            Note::new(62, 0.25962179, 113),
            Note::new(62, 0.229479448, 0),
            Note::new(65, 0.317320844, 4),
            Note::new(65, 0.042830378, 0),
            Note::new(65, 0.582655121, 70),
            Note::new(65, 0.50379576, 0),
            Note::new(65, 0.250825755, 106),
            Note::new(65, 0.017210736, 0),
            Note::new(64, 0.272029135, 100),
            Note::new(64, 0.027428442, 0),
            Note::new(65, 1.126041184, 99),
            Note::new(64, 0.548192689, 99),
            Note::new(65, 0.273066185, 99),
            Note::new(64, 0.60045823, 117),
            Note::new(64, 0.450277594, 0),
            Note::new(64, 0.269494265, 85),
            Note::new(64, 0.003552609, 0),
            Note::new(62, 0.267746147, 96),
            Note::new(62, 0.016828202, 0),
            Note::new(64, 0.382390025, 123),
            Note::new(64, 0.128571533, 0),
            Note::new(64, 0.699718069, 113),
            Note::new(64, 0.126759354, 0),
            Note::new(62, 0.867493649, 117),
            Note::new(62, 0.46433006, 0),
            Note::new(64, 0.268483555, 106),
            Note::new(60, 1.323782698, 106),
            Note::new(60, 0.247095603, 100),
            Note::new(60, 0.026361804, 0),
            Note::new(64, 0.312539865, 30),
            Note::new(64, 0.008570104, 0),
            Note::new(62, 0.267542603, 100),
            Note::new(62, 0.05672056, 0),
            Note::new(65, 0.60457732, 110),
            Note::new(65, 0.537155291, 0),
            Note::new(65, 0.271879248, 113),
            Note::new(65, 0.06866826, 0),
            Note::new(67, 0.253815119, 88),
            Note::new(67, 0.10896619, 0),
            Note::new(65, 1.4822801190000001, 78),
            Note::new(67, 0.362781309, 78),
            Note::new(65, 0.202632925, 78),
            Note::new(64, 0.651313167, 118),
            Note::new(64, 0.412422427, 0),
            Note::new(64, 0.315570477, 86),
            Note::new(64, 0.032453773, 0),
            Note::new(57, 1.254315847, 92),
            Note::new(65, 0.321109969, 92),
            Note::new(64, 0.64096164, 117),
            Note::new(64, 0.485079544, 0),
            Note::new(55, 0.530547672, 94),
            Note::new(55, 0.017645017, 0),
            Note::new(62, 0.263664442, 125),
            Note::new(62, 0.009401743, 0),
            Note::new(60, 0.670999911, 111),
            Note::new(60, 1.5000003039999998, 0),
        ]);
        assert_eq!(melody, expected);
    }

    #[test]
    fn test_next_pitch() {
        let scale = MusicMode::new(ModNumC::new(0), 0);
        let cs = [
            -12, -10, -8, -7, -5, -3, -1, 0, 2, 4, 5, 7, 9, 11, 12, 14, 16, 17, 19, 21, 23, 24,
        ];
        for (i, p) in cs.iter().enumerate().skip(1) {
            assert_eq!(scale.next_pitch(cs[i - 1], DiatonicInterval::pure(1)), *p);
        }
        let next_pitch = scale.next_pitch(scale.root(), DiatonicInterval::pure(-6));
        assert_eq!(next_pitch, -10);
        let steps_between = scale.diatonic_steps_between(scale.root(), next_pitch);
        assert_eq!(steps_between, DiatonicInterval::pure(-6))
    }

    #[test]
    fn diatonic_step_bug() {
        let scale = MusicMode::new(ModNumC::new(0), 0);
        assert_eq!(scale.half_steps_up_to_scale(-10), 0);
        assert_eq!(scale.half_steps_up_to_scale(0), 0);
        assert_eq!(
            scale.diatonic_steps_between(-10, 0),
            DiatonicInterval::pure(6)
        );
    }

    #[test]
    fn test_normalize_interval() {
        let intervals = vec![
            DiatonicInterval {
                degree: -1,
                chroma: 0,
            },
            DiatonicInterval {
                degree: -5,
                chroma: 0,
            },
        ];
        let sum = intervals.iter().copied().sum::<DiatonicInterval>();
        assert_eq!(
            sum,
            DiatonicInterval {
                degree: -6,
                chroma: 0
            }
        );
        let scale = MusicMode::new(ModNumC::new(0), 0);
        let normalized = sum.normalized(&scale);
        assert_eq!(
            normalized,
            DiatonicInterval {
                degree: -6,
                chroma: 0
            }
        );
    }

    #[test]
    fn test_without_brief_notes() {
        let melody = Melody::from_vec(&vec![
            Note::new(72, 0.369792827, 127),
            Note::new(72, 0.061621093, 0),
            Note::new(74, 0.329988672, 127),
            Note::new(74, 0.127228427, 0),
            Note::new(76, 0.3714564, 99),
            Note::new(76, 0.085686124, 0),
            Note::new(77, 0.330959396, 127),
            Note::new(77, 0.159859305, 0),
            Note::new(72, 0.314343867, 76),
            Note::new(72, 0.026874508, 0),
            Note::new(74, 0.037653197, 12),
            Note::new(74, 0.044087338, 0),
            Note::new(75, 0.024298299, 77),
            Note::new(75, 0.000002524, 0),
            Note::new(74, 0.280279047, 80),
            Note::new(74, 0.178904154, 0),
            Note::new(76, 0.323056836, 89),
            Note::new(76, 0.1009156, 0),
            Note::new(77, 0.330098162, 127),
            Note::new(77, 0.10557298, 0),
            Note::new(72, 0.300934497, 80),
            Note::new(72, 0.00000287, 0),
            Note::new(73, 0.022893581, 20),
            Note::new(73, 0.044463414, 0),
            Note::new(74, 0.394669786, 101),
            Note::new(74, 0.078055973, 0),
            Note::new(77, 0.025097277, 91),
            Note::new(77, 0.00000256, 0),
            Note::new(76, 0.332283936, 92),
            Note::new(76, 0.113469215, 0),
            Note::new(77, 0.345122384, 127),
            Note::new(77, 1.5000002829999999, 0),
        ]);
        let expected = Melody::from_vec(&vec![
            Note::new(72, 0.369792827, 127),
            Note::new(72, 0.061621093, 0),
            Note::new(74, 0.329988672, 127),
            Note::new(74, 0.127228427, 0),
            Note::new(76, 0.3714564, 99),
            Note::new(76, 0.085686124, 0),
            Note::new(77, 0.330959396, 127),
            Note::new(77, 0.159859305, 0),
            Note::new(72, 0.314343867, 76),
            Note::new(72, 0.026874508, 0),
            Note::new(74, 0.280279047, 80),
            Note::new(74, 0.178904154, 0),
            Note::new(76, 0.323056836, 89),
            Note::new(76, 0.1009156, 0),
            Note::new(77, 0.330098162, 127),
            Note::new(77, 0.10557298, 0),
            Note::new(72, 0.300934497, 80),
            Note::new(72, 0.00000287, 0),
            Note::new(74, 0.394669786, 101),
            Note::new(74, 0.078055973, 0),
            Note::new(76, 0.332283936, 92),
            Note::new(76, 0.113469215, 0),
            Note::new(77, 0.345122384, 127),
            Note::new(77, 1.5000002829999999, 0),
        ]);
        assert_eq!(melody.without_brief_notes(0.1), expected);
    }

    fn lean_on_me_melody() -> Melody {
        let mut melody = Melody::new();
        for (pitch, duration, velocity) in LEAN_ON_ME.iter().copied() {
            melody.add(Note::new(pitch, duration, velocity));
        }
        assert_eq!(melody.len(), LEAN_ON_ME.len());
        melody
    }

    #[test]
    fn test_ornamentation() {
        let maker = MelodyMaker::new();
        let melody = lean_on_me_melody();
        let scale = melody.best_scale_for();
        for _ in 0..NUM_RANDOM_TESTS {
            let ornamented = maker.ornamented(&scale, &melody, 1.0);
            assert!(ornamented.len() > melody.len());
            assert_approx_eq!(f64, melody.duration(), ornamented.duration());
            let cm = melody.get_consolidated_notes();
            let om = ornamented.get_consolidated_notes();
            let mut ornaments = 0;
            for i in 0..cm.len() {
                assert!(i + ornaments < om.len());
                if om[i + ornaments].1.pitch != cm[i].1.pitch {
                    ornaments += 1;
                }
            }
            println!("{ornaments} ({})", melody.len());
        }
    }

    #[test]
    fn test_whimsified_ending() {
        let maker = MelodyMaker::new();
        let melody = lean_on_me_melody();
        let mut num_different = 0;
        for _ in 0..NUM_RANDOM_TESTS {
            let whimsified = maker.whimsified_ending(&melody);
            assert_eq!(melody.len(), whimsified.len());
            assert!(melody.common_prefix_length(&whimsified) >= melody.len() - 8);
            if melody != whimsified {
                num_different += 1;
            }
        }
        assert!(num_different >= NUM_RANDOM_TESTS - 1);
    }

    #[test]
    fn test_melody_direction() {
        let melody = lean_on_me_melody();
        for (f, s, i, expected) in [
            (
                MelodicFigure {
                    shape: MelodicFigureShape::Note3Scale,
                    polarity: FigurePolarity::Positive,
                    direction: FigureDirection::Forward,
                },
                64,
                0,
                MelodyDirection::Away,
            ),
            (
                MelodicFigure {
                    shape: MelodicFigureShape::Note3Scale,
                    polarity: FigurePolarity::Positive,
                    direction: FigureDirection::Forward,
                },
                60,
                0,
                MelodyDirection::Toward,
            ),
            (
                MelodicFigure {
                    shape: MelodicFigureShape::ReturnCrazyDriver,
                    polarity: FigurePolarity::Positive,
                    direction: FigureDirection::Forward,
                },
                60,
                0,
                MelodyDirection::Toward,
            ),
            (
                MelodicFigure {
                    shape: MelodicFigureShape::Arpeggio,
                    polarity: FigurePolarity::Positive,
                    direction: FigureDirection::Forward,
                },
                60,
                0,
                MelodyDirection::Toward,
            ),
            (
                MelodicFigure {
                    shape: MelodicFigureShape::Arch,
                    polarity: FigurePolarity::Positive,
                    direction: FigureDirection::Forward,
                },
                60,
                0,
                MelodyDirection::Toward,
            ),
            (
                MelodicFigure {
                    shape: MelodicFigureShape::Arch,
                    polarity: FigurePolarity::Negative,
                    direction: FigureDirection::Forward,
                },
                60,
                0,
                MelodyDirection::Away,
            ),
        ] {
            assert_eq!(expected, MelodyDirection::find(f, s, &melody, i));
        }
    }

    #[test]
    fn test_melody_note_ranking() {
        let melody = lean_on_me_melody();
        let ranked = melody.notes_ranked_by_duration();
        assert_eq!(
            ranked,
            [
                (45, Note::new(65, 1.4822801190000001, 78)),
                (32, Note::new(60, 1.323782698, 106)),
                (52, Note::new(57, 1.254315847, 92)),
                (16, Note::new(62, 1.126041184, 99)),
                (29, Note::new(62, 0.867493649, 117)),
                (27, Note::new(64, 0.699718069, 113)),
                (60, Note::new(60, 0.670999911, 111)),
                (48, Note::new(64, 0.651313167, 118)),
                (54, Note::new(64, 0.64096164, 117)),
                (39, Note::new(65, 0.60457732, 110)),
                (19, Note::new(60, 0.60045823, 117)),
                (10, Note::new(65, 0.582655121, 70)),
                (17, Note::new(64, 0.548192689, 99)),
                (56, Note::new(55, 0.530547672, 94)),
                (0, Note::new(60, 0.487445772, 92)),
                (25, Note::new(64, 0.382390025, 123)),
                (46, Note::new(64, 0.362781309, 78)),
                (53, Note::new(65, 0.321109969, 92)),
                (8, Note::new(65, 0.317320844, 4)),
                (50, Note::new(64, 0.315570477, 86)),
                (35, Note::new(62, 0.312539865, 30)),
                (2, Note::new(60, 0.289316858, 93)),
                (18, Note::new(62, 0.273066185, 99)),
                (14, Note::new(64, 0.272029135, 100)),
                (41, Note::new(65, 0.271879248, 113)),
                (21, Note::new(60, 0.269494265, 85)),
                (31, Note::new(64, 0.268483555, 106)),
                (23, Note::new(62, 0.267746147, 96)),
                (37, Note::new(64, 0.267542603, 100)),
                (58, Note::new(62, 0.263664442, 125)),
                (6, Note::new(64, 0.25962179, 113)),
                (43, Note::new(64, 0.253815119, 88)),
                (12, Note::new(65, 0.250825755, 106)),
                (4, Note::new(62, 0.248933836, 102)),
                (33, Note::new(60, 0.247095603, 100)),
                (47, Note::new(62, 0.202632925, 78))
            ]
        );
    }
}
