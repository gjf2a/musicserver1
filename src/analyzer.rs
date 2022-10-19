use bare_metal_modulo::{MNum, ModNumC};
use enum_iterator::{all, Sequence};
use float_cmp::{ApproxEq, F64Margin};
use histogram_macros::*;
use midi_msg::MidiMsg::ChannelVoice;
use midi_msg::{Channel, ChannelVoiceMsg, MidiMsg};
use ordered_float::OrderedFloat;
use rand::prelude::SliceRandom;
use std::cmp::{max, min};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::time::Instant;

pub type MidiByte = i16;

const MAX_MIDI_VALUE: MidiByte = i8::MAX as MidiByte;
const NOTES_PER_OCTAVE: MidiByte = 12;
const USIZE_NOTES_PER_OCTAVE: usize = NOTES_PER_OCTAVE as usize;
const DIATONIC_SCALE_SIZE: usize = 7;
const DIATONIC_SCALE_HOPS: [MidiByte; DIATONIC_SCALE_SIZE] = [2, 2, 1, 2, 2, 2, 1];

const NOTE_NAMES: [&str; NOTES_PER_OCTAVE as usize] = [
    "C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B",
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

fn major_sharps_for(note_index: usize) -> usize {
    if note_index % 2 == 0 {
        note_index
    } else {
        (note_index + 6) % NOTES_PER_OCTAVE as usize
    }
}

fn major_flats_for(note_index: usize) -> usize {
    (NOTES_PER_OCTAVE as usize + (if note_index % 2 == 0 {0} else {6}) - note_index) % NOTES_PER_OCTAVE as usize
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

const FIGURE_LENGTHS: [usize; 2] = [4, 3];

fn assert_prob(p: f64) {
    assert!(0.0 <= p && p <= 1.0);
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
            && self.duration.into_inner().approx_eq(other.duration.into_inner(), margin)
    }
}

impl Note {
    pub fn new(pitch: MidiByte, duration: f64, velocity: MidiByte) -> Self {
        Note {pitch, duration: OrderedFloat(duration), velocity}
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

    pub fn duration(&self) -> f64 {self.duration.into_inner()}

    pub fn velocity(&self) -> MidiByte {self.velocity}

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

#[derive(Copy, Clone)]
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
        Note {
            pitch: self.pitch as MidiByte,
            duration: OrderedFloat(0.0),
            velocity: 0,
        }
    }
}

impl From<PendingNote> for Note {
    fn from(pending_note: PendingNote) -> Self {
        Note {
            pitch: pending_note.pitch as MidiByte,
            duration: OrderedFloat(pending_note.elapsed()),
            velocity: pending_note.velocity as MidiByte,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Melody {
    notes: Vec<Note>,
}

impl Melody {
    pub fn new() -> Self {
        Melody { notes: vec![] }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Note> {
        self.notes.iter()
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
        if result.len() == length {Some(result)} else {None}
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

    pub fn from(s: &str) -> Self {
        let mut notes = Vec::new();
        let mut nums = s.split(",");
        while let Some(note) = nums.next() {
            let note = note.parse().unwrap();
            let duration = nums.next().unwrap().parse().unwrap();
            let f_intensity: OrderedFloat<f64> = nums.next().unwrap().parse().unwrap();
            let intensity = (f_intensity.into_inner() * MAX_MIDI_VALUE as f64) as MidiByte;
            notes.push(Note {
                pitch: note,
                duration,
                velocity: intensity,
            });
        }
        Melody { notes }
    }

    pub fn add(&mut self, n: Note) {
        self.notes.push(n);
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
        let mut result = String::new();
        for n in self.notes.iter() {
            result.push_str(
                format!(
                    "{} [({:2}) ({:2})] ",
                    NOTE_NAMES[(n.pitch % NOTES_PER_OCTAVE) as usize],
                    n.duration,
                    n.velocity
                )
                .as_str(),
            );
        }
        result
    }

    pub fn find_root_pitch(&self) -> MidiByte {
        let note_iter = self
            .notes
            .iter()
            .map(|n| (n.pitch % NOTES_PER_OCTAVE, n.duration));
        let note_weights = collect_from_by_into!(note_iter, HashMap::new());
        mode_by_weight!(note_weights).unwrap()
    }

    pub fn best_scale_for(&self) -> MusicMode {
        let mut mode_weights = HashMap::new();
        for mode in MusicMode::all_modes_for(self.find_root_pitch()).iter() {
            for n in self.notes.iter().filter(|n| mode.contains(n.pitch)) {
                bump_ref_by!(mode_weights, mode, n.duration);
            }
        }
        mode_by_weight!(mode_weights).unwrap()
    }

    pub fn diatonic_intervals(&self) -> Vec<Option<MidiByte>> {
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
}

impl std::ops::Index<usize> for Melody {
    type Output = Note;

    fn index(&self, index: usize) -> &Self::Output {
        &self.notes[index]
    }
}

pub struct MelodyMaker {
    figure_tables: BTreeMap<usize, BTreeMap<MidiByte, Vec<MelodicFigure>>>,
    figure_mappings: HashMap<MelodicFigure, MelodicFigure>,
}

#[derive(Copy,Clone)]
struct Neighbor {
    gap: MidiByte,
    prev_pitch: MidiByte,
    velocity: MidiByte,
    duration: f64
}

impl Neighbor {
    fn new(melody: &Melody, scale: &MusicMode, i: usize) -> Option<Self> {
        if i == 0 || i >= melody.len() {
            None
        } else {
            scale.diatonic_steps_between(melody[i - 1].pitch, melody[i].pitch)
                .map(|s| Neighbor {gap: s, prev_pitch: melody[i - 1].pitch, velocity: melody[i].velocity, duration: melody.duration_with_rest(i).into_inner()})
        }
    }

    fn add_ornament_pitches(&self, result: &mut Melody, scale: &MusicMode, figure: MelodicFigure) {
        let ornament_pitches = self.make_ornament_pitches(figure, scale);
        let duration = OrderedFloat(self.duration / ornament_pitches.len() as f64);
        for pitch in ornament_pitches {
            result.add(Note {pitch, duration, velocity: self.velocity});
        }
    }

    fn make_ornament_pitches(&self, figure: MelodicFigure, scale: &MusicMode) -> VecDeque<MidiByte> {
        let mut ornament_pitches = figure.make_pitches(self.prev_pitch, scale);
        ornament_pitches.pop_front();
        ornament_pitches
    }
}

impl MelodyMaker {
    pub fn new() -> Self {
        MelodyMaker {
            figure_tables: FIGURE_LENGTHS
                .iter()
                .map(|len| (*len, MelodicFigure::interval2figures(*len)))
                .collect(),
            figure_mappings: HashMap::new(),
        }
    }

    /// Finds a MelodicFigure that matches the pitch sequence in `melody` starting at `start`.
    /// Prefers matching a 4-note figure to a 3-note figure.
    pub fn matching_figure(&self, melody: &Melody, start: usize) -> Option<(MelodicFigure, usize)> {
        let scale = melody.best_scale_for();
        for length in FIGURE_LENGTHS.iter() {
            if let Some(pitches) = melody.pitch_subsequence_at(start, *length) {
                if let Some(step_gap) = scale.diatonic_steps_between(*pitches.first().unwrap(), *pitches.last().unwrap()) {
                    if let Some(candidates) = self.figure_tables.get(length).unwrap().get(&step_gap) {
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

    pub fn ornamented(&self, melody: &Melody, p_ornament: f64, ornament_gap: i64) -> Melody {
        if melody.len() == 0 {
            return melody.clone();
        }
        let min_duration = melody.median_duration_note_on();
        let scale = melody.best_scale_for();
        let mut result = Melody::new();
        let mut countup = 1;
        let mut i = 0;
        while i < melody.len() {
            let mut ornamented = false;
            if !melody[i].is_rest() {
                let neighbor = Neighbor::new(&melody, &scale, i);
                if let Some(neighbor) = neighbor {
                    if let Some(figure) = self.choose_ornament_figure(neighbor.gap) {
                        if melody.duration_with_rest(i) >= min_duration {
                            let p_choose = p_ornament * countup as f64 / ornament_gap as f64;
                            if rand::random::<f64>() < p_choose {
                                neighbor.add_ornament_pitches(&mut result, &scale, figure);
                                ornamented = true;
                                countup = 1;
                                i += 1;
                                while i < melody.len() && melody[i].is_rest() {i += 1;}
                            } else {
                                countup += 1;
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
        let end = result.last_note();
        if !end.is_rest() {
            result.add(Note {pitch: end.pitch, velocity: 0, duration: end.duration});
        }
        result
    }

    fn random_element_from<T: Copy>(v: &Vec<T>) -> Option<T> {
        match v.len() {
            0 => None,
            _ => Some(v[rand::random::<usize>() % v.len()])
        }
    }

    fn choose_ornament_figure(&self, gap: MidiByte) -> Option<MelodicFigure> {
        let options = self.figure_tables.values()
            .filter_map(|t| t.get(&gap))
            .map(|v| v[rand::random::<usize>() % v.len()])
            .collect::<Vec<_>>();
        Self::random_element_from(&options)
    }

    pub fn is_chain(chain: &VecDeque<(usize, Option<(MelodicFigure, usize)>)>) -> bool {
        let mut current = 0;
        for (i, f) in chain.iter() {
            if *i != current {
                return false;
            } else {
                current += f.map_or(1, |(_, f_len)| f_len - 1);
            }
        }
        true
    }

    pub fn all_figure_matches(&self, melody: &Melody) -> Vec<(usize, MelodicFigure, usize)> {
        (0..melody.len())
            .filter_map(|i| self.matching_figure(melody, i).map(|f| (i, f.0, f.1)))
            .collect()
    }

    pub fn emphasis_figure_chain(
        &self,
        melody: &Melody,
        emphasized_indices: &Vec<usize>,
    ) -> VecDeque<(usize, Option<(MelodicFigure, usize)>)> {
        let all_matched = self.all_figure_matches(melody);
        let emphasized_indices: BTreeSet<usize> = emphasized_indices.iter().copied().collect();
        let mut keepers = BTreeMap::new();
        self.add_if_clear(&all_matched, &mut keepers, |s, l| {
            emphasized_indices.contains(&(s + l - 1))
        });
        self.add_if_clear(&all_matched, &mut keepers, |s, _l| {
            emphasized_indices.contains(&s)
        });
        self.add_if_clear(&all_matched, &mut keepers, |_s, _l| true);
        Self::keepers2chain(&keepers, melody)
    }

    pub fn locked_in_figures(&self, melody: &Melody) -> VecDeque<(usize, Option<(MelodicFigure, usize)>)> {
        let all_matched = self.all_figure_matches(melody);
        let mut ranked = Self::all_figure_matches_ranked(&all_matched);
        let mut rng = rand::thread_rng();
        let mut keepers = BTreeMap::new();
        while ranked.len() > 0 {
            let mut candidates = Self::take_most_common_figures(&mut ranked);
            candidates.shuffle(&mut rng);
            while candidates.len() > 0 {
                let figure = candidates.pop().unwrap();
                for (start, len) in all_matched.iter()
                    .filter(|(_, f, _)| *f == figure)
                    .map(|(i, _, l)| (*i, *l))
                {
                    self.add_no_interference(&mut keepers, figure, start, len);
                }
            }
        }
        Self::keepers2chain(&keepers, melody)
    }

    fn all_figure_matches_ranked(all_matched: &Vec<(usize, MelodicFigure, usize)>) -> VecDeque<(MelodicFigure, usize)> {
        let figure2count = collect_from_into!(
            all_matched.iter().copied().map(|(_, f, _)| f),
            HashMap::<MelodicFigure, usize>::new()
        );
        ranking!(figure2count)
    }

    fn take_most_common_figures(ranked: &mut VecDeque<(MelodicFigure, usize)>) -> Vec<MelodicFigure> {
        let max_count = ranked[0].1;
        let mut candidates = HashSet::new();
        while ranked.len() > 0 && ranked[0].1 == max_count {
            candidates.insert(ranked.pop_front().unwrap().0);
        }
        candidates.iter().copied().collect()
    }

    fn keepers2chain(keepers: &BTreeMap<usize, (MelodicFigure, usize)>, melody: &Melody) -> VecDeque<(usize, Option<(MelodicFigure, usize)>)> {
        let mut covered = HashSet::new();
        let mut result = VecDeque::new();
        for i in 0..melody.len() {
            if !covered.contains(&i) {
                result.push_back((
                    i,
                    match keepers.get(&i) {
                        None => {
                            covered.insert(i);
                            None
                        }
                        Some((f, f_len)) => {
                            for c in 0..(*f_len - 1) {
                                covered.insert(i + c);
                            }
                            Some((*f, *f_len))
                        }
                    },
                ));
            }
        }
        result
    }

    fn add_if_clear<P: Fn(usize, usize) -> bool>(
        &self,
        all_matched: &Vec<(usize, MelodicFigure, usize)>,
        keepers: &mut BTreeMap<usize, (MelodicFigure, usize)>,
        filter: P,
    ) {
        for (start, figure, len) in all_matched.iter() {
            if filter(*start, *len) {
                self.add_no_interference(keepers, *figure, *start, *len);
            }
        }
    }

    pub fn max_figure_len(&self) -> usize {
        *self.figure_tables.keys().max().unwrap()
    }

    fn add_no_interference(
        &self,
        keepers: &mut BTreeMap<usize, (MelodicFigure, usize)>,
        figure: MelodicFigure,
        start: usize,
        len: usize,
    ) {
        let mut prev_zone = 0..(start + len);
        if !prev_zone.any(|i| {
            keepers
                .get(&i)
                .map_or(false, |(_, f_len)| figure.interfere(start, len, i, *f_len))
        }) {
            keepers.insert(start, (figure, len));
        }
    }

    fn chain_variation_creator<C, P>(
        &mut self,
        original: &Melody,
        p_rewrite: f64,
        chain_maker: C,
        mut figure_picker: P,
    ) -> Melody
    where
        C: Fn(&Self, &Melody) -> VecDeque<(usize, Option<(MelodicFigure, usize)>)>,
        P: FnMut(&mut Self, MelodicFigure) -> MelodicFigure,
    {
        assert_prob(p_rewrite);
        let scale = original.best_scale_for();
        let mut figure_chain = chain_maker(self, &original);
        assert!(Self::is_chain(&figure_chain));
        let mut notes = vec![];
        let mut next_already_added = false;
        while let Some((i, figure)) = figure_chain.pop_front() {
            if !next_already_added {
                notes.push(original[i]);
            }
            next_already_added = match figure {
                None => false,
                Some((figure, match_len)) => {
                    self.push_notes_for(&mut notes, figure, match_len, p_rewrite, &mut figure_picker, i, original, &scale);
                    true
                }
            };
        }
        Melody { notes }
    }

    fn push_notes_for<P: FnMut(&mut Self, MelodicFigure) -> MelodicFigure>(&mut self, notes: &mut Vec<Note>, figure: MelodicFigure, match_len: usize, p_rewrite: f64,
                      figure_picker: &mut P, i: usize, original: &Melody, scale: &MusicMode) {
        let generator = if rand::random::<f64>() < p_rewrite {
            figure_picker(self, figure)
        } else {
            figure
        };
        let pitches = generator.make_pitches(notes.last().unwrap().pitch, &scale);
        let mut pitch = 0;
        for m in 1..match_len {
            if original[i + m].pitch != original[i + m - 1].pitch {
                pitch += 1;
            }
            notes.push(original[i + m].repitched(pitches[pitch]));
        }
    }

    pub fn pick_figure(&mut self, figure: MelodicFigure) -> MelodicFigure {
        let figure_length = figure.len();
        let jump = figure.total_change();
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

    pub fn create_emphasis_variation(&mut self, original: &Melody, p_rewrite: f64) -> Melody {
        self.chain_variation_creator(
            original,
            p_rewrite,
            |s, m| s.emphasis_figure_chain(m, &m.find_pause_indices()),
            Self::pick_figure,
        )
    }

    pub fn create_figure_mapped_emphasis_variation(&mut self, original: &Melody, p_rewrite: f64) -> Melody {
        self.chain_variation_creator(
            original,
            1.0,
            |s, m| s.emphasis_figure_chain(m, &m.find_pause_indices()),
            |s, f| s.pick_remembered_figure(f, p_rewrite),
        )
    }

    pub fn create_figure_mapped_variation(&mut self, original: &Melody, p_remap: f64) -> Melody {
        assert_prob(p_remap);
        self.reset_figure_mappings();
        self.chain_variation_creator(
            original,
            1.0,
            |s, m| s.locked_in_figures(m),
            |s, f| s.pick_remembered_figure(f, p_remap),
        )
    }

    fn pick_remembered_figure(&mut self, figure: MelodicFigure, p_remap: f64) -> MelodicFigure {
        match self.figure_mappings.get(&figure) {
            None => {
                let r = if rand::random::<f64>() < p_remap {
                    self.pick_figure(figure)
                } else {
                    figure
                };
                self.figure_mappings.insert(figure, r);
                r
            }
            Some(r) => *r,
        }
    }

    fn reset_figure_mappings(&mut self) {
        self.figure_mappings = HashMap::new();
    }

    pub fn print_figure_mappings(&self) {
        for (original, mapped) in self.figure_mappings.iter() {
            println!("{:?} -> {:?}", original, mapped);
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct MusicMode {
    root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>,
    octave_notes: [ModNumC<MidiByte, USIZE_NOTES_PER_OCTAVE>; DIATONIC_SCALE_SIZE],
}

impl MusicMode {
    pub fn all_modes_for(root_note: MidiByte) -> Vec<Self> {
        (0..DIATONIC_SCALE_SIZE)
            .map(|i| Self::new(ModNumC::new(i), root_note))
            .collect()
    }

    pub fn name(&self) -> String {
        let mut note_name = NOTE_NAMES[self.root() as usize];
        if note_name.len() > 1 {
            note_name = if sharps_for(self.root() as usize, self.root_pos.a()) > flats_for(self.root() as usize, self.root_pos.a()) {
                &note_name[..2]
            } else {
                &note_name[3..]
            };
        }
        format!("{} {}", note_name, MODE_NAMES[self.root_pos.a()])
    }

    pub fn new(root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>, root_note: MidiByte) -> Self {
        let mut octave_notes = [ModNumC::new(root_note); DIATONIC_SCALE_SIZE];
        let mut offset = DIATONIC_SCALE_HOPS[root_pos.a()];
        for i in root_pos.iter().skip(1) {
            octave_notes[i.a()] += offset;
            offset += DIATONIC_SCALE_HOPS[i.a()];
        }
        MusicMode {
            root_pos,
            octave_notes,
        }
    }

    pub fn diatonic_steps_between(&self, pitch1: MidiByte, pitch2: MidiByte) -> Option<MidiByte> {
        assert!(pitch1 < i8::MAX as MidiByte + NOTES_PER_OCTAVE);
        if pitch1 > pitch2 {
            self.diatonic_steps_between(pitch2, pitch1)
                .map(|steps| -steps)
        } else if !self.contains(pitch1) || !self.contains(pitch2) {
            None
        } else {
            let mut count = 0;
            let mut p = pitch1;
            while p != pitch2 {
                p = self.next_pitch(p, 1);
                assert!(self.contains(p));
                count += 1;
            }
            Some(count)
        }
    }

    /// Returns the diatonic pitch `scale_steps_away` from `reference_pitch`.
    /// Panics if `reference_pitch` is not part of `self`'s scale.
    pub fn next_pitch(&self, reference_pitch: MidiByte, scale_steps_away: MidiByte) -> MidiByte {
        assert!(reference_pitch < MidiByte::MAX);
        let mut octaves_up = reference_pitch / NOTES_PER_OCTAVE;
        self.octave_notes
            .iter()
            .position(|p| *p == reference_pitch)
            .map(|i| {
                let ref_octave_basis = self.octave_notes[i].a();
                let j: ModNumC<MidiByte, DIATONIC_SCALE_SIZE> =
                    ModNumC::new(i as MidiByte + scale_steps_away);
                let next_octave_basis = self.octave_notes[j.a() as usize].a();
                if scale_steps_away > 0 && ref_octave_basis > next_octave_basis {
                    octaves_up += 1
                } else if scale_steps_away < 0 && ref_octave_basis < next_octave_basis {
                    octaves_up -= 1;
                }
                octaves_up += scale_steps_away / 7;
                next_octave_basis + octaves_up * NOTES_PER_OCTAVE
            })
            .unwrap()
    }

    fn root(&self) -> MidiByte {
        self.octave_notes[self.root_pos.a()].a()
    }

    pub fn contains(&self, pitch: MidiByte) -> bool {
        self.octave_notes.contains(&(ModNumC::new(pitch)))
    }

    pub fn diatonic_degree(&self, pitch: MidiByte) -> Option<MidiByte> {
        let mut pertinent_root = self.root();
        while pertinent_root + NOTES_PER_OCTAVE <= pitch {
            pertinent_root += NOTES_PER_OCTAVE;
        }
        self.diatonic_steps_between(pertinent_root, pitch)
            .map(|d| d + 1)
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
    pub fn total_change(&self) -> MidiByte {
        self.pattern().iter().sum()
    }

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

    pub fn match_length(&self, melody: &Melody, scale: &MusicMode, start: usize) -> Option<usize> {
        let mut p = 0;
        let mut m = start;
        while p < self.pattern().len() && m + 1 < melody.len() {
            if melody[m].pitch != melody[m + 1].pitch {
                match scale.diatonic_steps_between(melody[m].pitch, melody[m + 1].pitch) {
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
        Some(m - start)
    }

    /// Generates a sequence of diatonic pitches derived from `scale` corresponding to
    /// `self.pattern()`. The sequence starts at `start_pitch` (which is included in the result).
    pub fn make_pitches(&self, start_pitch: MidiByte, scale: &MusicMode) -> VecDeque<MidiByte> {
        let mut result = VecDeque::from([start_pitch]);
        for interval in self.pattern() {
            result.push_back(scale.next_pitch(*result.back().unwrap(), interval));
        }
        result
    }

    pub fn interval2figures(figure_length: usize) -> BTreeMap<MidiByte, Vec<Self>> {
        let mut result = BTreeMap::new();
        for m in all::<MelodicFigure>() {
            if m.len() == figure_length {
                let interval = m.total_change();
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
    use crate::analyzer::{FigureDirection, FigurePolarity, MelodicFigure, MelodicFigureShape, Melody, MelodyMaker, MusicMode, DIATONIC_SCALE_SIZE, major_sharps_for, major_flats_for, sharps_for, flats_for};
    use crate::{MidiByte, Note};
    use bare_metal_modulo::ModNumC;
    use ordered_float::OrderedFloat;
    use std::cmp::{max, min};
    use std::collections::{BTreeSet, VecDeque};

    const EXAMPLE_MELODY: &str = "55,0.39,0.91,55,0.04,0.0,59,0.33,0.73,60,0.06,0.44,62,0.02,0.87,59,0.05,0.0,60,0.16,0.0,62,0.2,0.0,55,0.39,0.61,55,0.01,0.0,57,0.34,0.98,57,0.05,0.0,55,0.39,0.78,54,0.02,0.98,55,0.19,0.0,54,0.12,0.0,52,0.11,0.74,52,0.0,0.0,54,0.12,0.46,54,0.03,0.0,50,0.1,0.84,50,0.27,0.0,55,0.27,0.74,55,0.1,0.0,59,0.27,0.44,60,0.07,0.54,62,0.04,0.91,59,0.09,0.0,60,0.11,0.0,62,0.19,0.0,55,0.29,0.67,55,0.07,0.0,57,0.32,0.76,57,0.06,0.0,55,0.23,0.7,55,0.05,0.0,54,0.12,0.93,54,0.07,0.0,50,0.37,0.8,50,0.5,0.0,55,0.36,0.76,55,0.05,0.0,59,0.28,0.76,60,0.05,0.7,62,0.01,0.91,59,0.07,0.0,60,0.15,0.0,62,0.2,0.0,55,0.33,0.67,55,0.02,0.0,57,0.29,0.8,57,0.1,0.0,55,0.29,0.9,55,0.08,0.0,54,0.16,1.0,54,0.12,0.0,52,0.12,0.72,54,0.01,0.71,52,0.14,0.0,54,0.07,0.0,50,0.1,0.76,50,0.23,0.0,55,0.22,0.65,55,0.13,0.0,57,0.29,0.64,57,0.08,0.0,55,0.23,0.76,55,0.07,0.0,54,0.12,0.99,54,0.04,0.0,52,0.24,0.95,52,0.19,0.0,54,0.13,1.0,54,0.15,0.0,52,0.12,0.72,52,0.03,0.0,54,0.19,0.83,54,0.13,0.0,50,0.06,0.69,50,0.15,0.0,55,0.01,0.73,57,0.07,0.66,57,0.55,0.0,55,1.5,0.0";
    const COUNTDOWN_MELODY: &str = "66,0.42,1.0,66,0.55,0.0,73,0.17,1.0,73,0.01,0.0,71,0.13,0.77,71,0.0,0.0,73,0.45,0.41,73,0.13,0.0,66,0.85,0.8,66,0.32,0.0,74,0.16,1.0,74,0.0,0.0,74,0.37,0.87,74,0.03,0.0,73,0.2,1.0,73,0.03,0.0,71,0.03,0.06,71,0.04,0.0,71,0.93,1.0,71,0.27,0.0,74,0.16,1.0,74,0.03,0.0,73,0.13,1.0,73,0.03,0.0,74,0.45,1.0,74,0.12,0.0,66,0.58,0.8,66,0.5,0.0,71,0.15,0.75,71,0.02,0.0,71,0.13,0.81,71,0.03,0.0,71,0.21,1.0,71,0.08,0.0,69,0.24,0.94,69,0.08,0.0,68,0.22,0.65,68,0.07,0.0,71,0.24,1.0,71,0.06,0.0,69,0.68,1.0,69,0.15,0.0,73,0.16,1.0,73,0.03,0.0,71,0.14,0.91,71,0.03,0.0,73,0.29,1.0,73,0.22,0.0,66,0.61,0.64,66,0.45,0.0,74,0.15,0.87,74,0.04,0.0,74,0.14,0.83,74,0.02,0.0,74,0.2,1.0,74,0.13,0.0,73,0.29,0.96,73,0.0,0.0,72,0.04,0.49,72,0.03,0.0,71,1.01,1.0,71,0.41,0.0,74,0.14,0.94,74,0.04,0.0,73,0.13,0.8,73,0.03,0.0,74,0.49,1.0,74,0.12,0.0,66,0.93,0.54,66,0.19,0.0,71,0.16,0.81,71,0.02,0.0,71,0.13,0.79,71,0.03,0.0,71,0.21,0.87,71,0.11,0.0,69,0.24,0.86,69,0.08,0.0,68,0.24,0.67,68,0.07,0.0,71,0.24,1.0,71,0.11,0.0,69,0.75,0.86,69,0.05,0.0,68,0.18,0.71,68,0.02,0.0,69,0.16,0.89,69,0.04,0.0,71,0.02,0.99,71,0.0,0.0,83,0.01,1.0,83,0.0,0.0,71,0.56,0.98,71,0.16,0.0,69,0.19,1.0,69,0.04,0.0,71,0.2,1.0,71,0.05,0.0,73,0.24,1.0,73,0.0,0.0,72,0.03,0.62,72,0.07,0.0,71,0.2,0.91,71,0.03,0.0,69,0.01,0.06,69,0.06,0.0,69,0.18,0.73,69,0.11,0.0,68,0.19,0.46,68,0.18,0.0,66,0.51,0.76,66,0.17,0.0,74,0.56,1.0,74,0.01,0.0,73,1.09,0.79,73,0.07,0.0,75,0.16,0.9,75,0.03,0.0,73,0.16,0.84,73,0.03,0.0,71,0.18,0.57,71,0.03,0.0,73,0.78,0.64,73,0.06,0.0,73,0.14,0.91,73,0.04,0.0,73,0.14,0.87,73,0.04,0.0,73,0.26,0.81,73,0.1,0.0,71,0.23,0.91,71,0.07,0.0,69,0.19,0.98,69,0.1,0.0,68,0.23,0.59,68,0.15,0.0,66,1.22,0.68,66,2.0,0.0";
    const COUNTDOWN_ECHO: &str = "[[66, 0.42, 1.0], [66, 0.55, 0.0], [73, 0.17, 1.0], [73, 0.01, 0.0], [71, 0.13, 0.76], [71, 0.0, 0.0], [73, 0.45, 0.40], [73, 0.13, 0.0], [66, 0.85, 0.79], [66, 0.32, 0.0], [74, 0.16, 1.0], [74, 0.0, 0.0], [74, 0.37, 0.86], [74, 0.03, 0.0], [73, 0.2, 1.0], [73, 0.03, 0.0], [71, 0.03, 0.05], [71, 0.04, 0.0], [71, 0.93, 1.0], [71, 0.27, 0.0], [74, 0.16, 1.0], [74, 0.03, 0.0], [73, 0.13, 1.0], [73, 0.03, 0.0], [74, 0.45, 1.0], [74, 0.12, 0.0], [66, 0.58, 0.79], [66, 0.5, 0.0], [71, 0.15, 0.74], [71, 0.02, 0.0], [71, 0.13, 0.80], [71, 0.03, 0.0], [71, 0.21, 1.0], [71, 0.08, 0.0], [69, 0.24, 0.93], [69, 0.08, 0.0], [68, 0.22, 0.64], [68, 0.07, 0.0], [71, 0.24, 1.0], [71, 0.06, 0.0], [69, 0.68, 1.0], [69, 0.15, 0.0], [73, 0.16, 1.0], [73, 0.03, 0.0], [71, 0.14, 0.90], [71, 0.03, 0.0], [73, 0.29, 1.0], [73, 0.22, 0.0], [66, 0.61, 0.63], [66, 0.45, 0.0], [74, 0.15, 0.86], [74, 0.04, 0.0], [74, 0.14, 0.82], [74, 0.02, 0.0], [74, 0.2, 1.0], [74, 0.13, 0.0], [73, 0.29, 0.95], [73, 0.0, 0.0], [72, 0.04, 0.48], [72, 0.03, 0.0], [71, 1.01, 1.0], [71, 0.41, 0.0], [74, 0.14, 0.93], [74, 0.04, 0.0], [73, 0.13, 0.79], [73, 0.03, 0.0], [74, 0.49, 1.0], [74, 0.12, 0.0], [66, 0.93, 0.53], [66, 0.19, 0.0], [71, 0.16, 0.80], [71, 0.02, 0.0], [71, 0.13, 0.78], [71, 0.03, 0.0], [71, 0.21, 0.86], [71, 0.11, 0.0], [69, 0.24, 0.85], [69, 0.08, 0.0], [68, 0.24, 0.66], [68, 0.07, 0.0], [71, 0.24, 1.0], [71, 0.11, 0.0], [69, 0.75, 0.85], [69, 0.05, 0.0], [68, 0.18, 0.70], [68, 0.02, 0.0], [69, 0.16, 0.88], [69, 0.04, 0.0], [71, 0.02, 0.98], [71, 0.0, 0.0], [83, 0.01, 1.0], [83, 0.0, 0.0], [71, 0.56, 0.97], [71, 0.16, 0.0], [69, 0.19, 1.0], [69, 0.04, 0.0], [71, 0.2, 1.0], [71, 0.05, 0.0], [73, 0.24, 1.0], [73, 0.0, 0.0], [72, 0.03, 0.61], [72, 0.07, 0.0], [71, 0.2, 0.90], [71, 0.03, 0.0], [69, 0.01, 0.05], [69, 0.06, 0.0], [69, 0.18, 0.72], [69, 0.11, 0.0], [68, 0.19, 0.45], [68, 0.18, 0.0], [66, 0.51, 0.75], [66, 0.17, 0.0], [74, 0.56, 1.0], [74, 0.01, 0.0], [73, 1.09, 0.78], [73, 0.07, 0.0], [75, 0.16, 0.89], [75, 0.03, 0.0], [73, 0.16, 0.83], [73, 0.03, 0.0], [71, 0.18, 0.56], [71, 0.03, 0.0], [73, 0.78, 0.63], [73, 0.06, 0.0], [73, 0.14, 0.90], [73, 0.04, 0.0], [73, 0.14, 0.86], [73, 0.04, 0.0], [73, 0.26, 0.80], [73, 0.1, 0.0], [71, 0.23, 0.90], [71, 0.07, 0.0], [69, 0.19, 0.97], [69, 0.1, 0.0], [68, 0.23, 0.58], [68, 0.15, 0.0], [66, 1.22, 0.67], [66, 2.0, 0.0]]";

    #[test]
    fn test_parse_melody() {
        let m = "69,0.24,1.0,69,0.09,0.0,72,0.31,1.0,72,0.08,0.0,71,0.29,0.69";
        let notes = Melody::from(m);
        println!("{}", notes.view_notes());
        assert_eq!(format!("{:?}", notes), "Melody { notes: [Note { pitch: 69, duration: OrderedFloat(0.24), velocity: 127 }, Note { pitch: 69, duration: OrderedFloat(0.09), velocity: 0 }, Note { pitch: 72, duration: OrderedFloat(0.31), velocity: 127 }, Note { pitch: 72, duration: OrderedFloat(0.08), velocity: 0 }, Note { pitch: 71, duration: OrderedFloat(0.29), velocity: 87 }] }");
    }

    fn test_natural_mode(root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>, notes: [MidiByte; 15]) {
        let mode = MusicMode::new(root_pos, notes[0]);
        println!("{} {:?}", mode.name(), mode);
        for (i, n) in notes.iter().enumerate() {
            let i = i as MidiByte;
            let next = mode.next_pitch(notes[0], i);
            assert_eq!(next, *n);
            assert!(mode.contains(*n));
            let prev = mode.next_pitch(*n, -i);
            assert_eq!(notes[0], prev);
            assert!(mode.contains(prev));
        }

        let mut prev = notes[0];
        for n in notes.iter().skip(1) {
            let next = mode.next_pitch(prev, 1);
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
    fn test_melody_maker() {
        let tune = Melody::from(EXAMPLE_MELODY);
        let scale = tune.best_scale_for();
        let mut maker = MelodyMaker::new();
        for _ in 0..20 {
            let var = maker.create_figure_mapped_variation(&tune, 0.9);
            assert_eq!(var.len(), tune.len());
            let mut different_count = 0;
            for i in 0..var.len() {
                assert_eq!(var[i].duration, tune[i].duration);
                assert_eq!(var[i].velocity, tune[i].velocity);
                assert!(!scale.contains(tune[i].pitch) || scale.contains(var[i].pitch));
                if var[i].pitch != tune[i].pitch {
                    different_count += 1;
                }
            }
            let portion_changed = different_count as f64 / var.len() as f64;
            println!("Comparison (changed {:.2}%):", portion_changed * 100.0);
            for (original, new) in tune.notes.iter().zip(var.notes.iter()) {
                print!("({}->{}) ", original.pitch, new.pitch);
            }
            println!();
            println!();
            // I need to rethink this assertion below...
            //assert!(0.25 < portion_changed && portion_changed < 0.75);
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
        };
        let steps = mode.diatonic_steps_between(54, 57);
        assert_eq!(steps.unwrap(), 2);
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
            let np = mode.next_pitch(reference_pitch, scale_steps_away);
            assert_eq!(np, expected);
        }
    }

    #[test]
    fn test_send_back() {
        let tune = Melody::from(COUNTDOWN_MELODY);
        assert_eq!(tune.sonic_pi_list(), COUNTDOWN_ECHO);
    }

    #[test]
    fn test_diatonic_degree() {
        let melody = Melody::from(EXAMPLE_MELODY);
        let scale = melody.best_scale_for();
        assert_eq!(scale.name(), "G Ionian");
        for (i, pitch) in [67, 69, 71, 72, 74, 76, 78, 79, 81, 83, 84, 86, 88, 90, 91]
            .iter()
            .enumerate()
        {
            assert_eq!(
                (i % DIATONIC_SCALE_SIZE) as MidiByte + 1,
                scale.diatonic_degree(*pitch).unwrap()
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
        let melody = Melody::from(melody_str);
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

    #[test]
    fn test_emphasized_countdown() {
        let expected_pauses = vec![8, 16, 26, 40, 48, 60, 68, 82, 86, 92, 96, 110, 114, 122];
        test_emphasized(COUNTDOWN_MELODY, &expected_pauses);
    }

    #[test]
    fn test_emphasized_1() {
        let expected_pauses = vec![8, 14, 32, 38, 50, 58, 64, 70, 76];
        test_emphasized(EXAMPLE_MELODY, &expected_pauses);
    }

    fn test_emphasized(melody_str: &str, expected_pauses: &Vec<usize>) {
        let melody = Melody::from(melody_str);
        let maker = MelodyMaker::new();
        let pauses = melody.find_pause_indices();
        assert_eq!(&pauses, expected_pauses);
        let chain = maker.emphasis_figure_chain(&melody, &pauses);
        assert!(MelodyMaker::is_chain(&chain));
        let all_figures = maker.all_figure_matches(&melody);
        let all_pause_figures = all_figures
            .iter()
            .filter(|(i, _, _)| pauses.contains(i))
            .copied()
            .collect::<Vec<_>>();

        let mut prev_end = 0;
        for (start, _, len) in all_pause_figures.iter() {
            assert!(prev_end > *start || chain.iter().find(|c| c.0 == *start).is_some());
            prev_end = *start + *len;
        }
    }

    fn test_variation_unchanged<V: Fn(&mut MelodyMaker, &Melody, f64) -> Melody>(v_func: V) {
        let melody = Melody::from(COUNTDOWN_MELODY);
        let mut maker = MelodyMaker::new();
        let v = v_func(&mut maker, &melody, 0.0);
        assert_eq!(v.len(), melody.len());
        assert_eq!(v.sonic_pi_list().as_str(), COUNTDOWN_ECHO);
    }

    fn test_variation_changed<V: Fn(&mut MelodyMaker, &Melody, f64) -> Melody>(
        v_func: V,
        expected_lo: f64,
        expected_hi: f64,
    ) {
        let melody = Melody::from(COUNTDOWN_MELODY);
        let scale = melody.best_scale_for();
        let mut maker = MelodyMaker::new();
        let mut lo = OrderedFloat(1.0);
        let mut hi = OrderedFloat(0.0);
        for _ in 0..20 {
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
    fn test_variation_2() {
        test_variation_unchanged(MelodyMaker::create_emphasis_variation);
        test_variation_changed(MelodyMaker::create_emphasis_variation, 0.40, 0.60);
    }

    #[test]
    fn test_variation_4() {
        test_variation_unchanged(MelodyMaker::create_figure_mapped_variation);
        test_variation_changed(MelodyMaker::create_figure_mapped_variation, 0.25, 0.55);
    }

    #[test]
    fn test_matching_figure() {
        let melody = Melody::from(COUNTDOWN_MELODY);
        let scale = melody.best_scale_for();

        let pitch1 = melody.pitch_subsequence_at(0, 4).unwrap();
        assert_eq!(pitch1, vec![66, 73, 71, 73]);
        let gap1 = scale
            .diatonic_steps_between(*pitch1.first().unwrap(), *pitch1.last().unwrap())
            .unwrap();
        assert_eq!(gap1, 4);
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
        let melody = Melody::from(COUNTDOWN_MELODY);
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
            m.add(Note {
                pitch: n,
                duration: OrderedFloat(d),
                velocity: i,
            });
        }
        assert!(!m.all_rests_synchronized());
        m.synchronize_rests();
        println!("{m:?}");
        assert!(m.all_rests_synchronized());
    }

    #[test]
    fn test_sharp_flat() {
        for (chrom, sharp, flat) in [(0, 0, 0), (2, 2, 10), (4, 4, 8), (5, 11, 1), (7, 1, 11), (9, 3, 9)].iter() {
            assert_eq!(major_sharps_for(*chrom), *sharp);
            assert_eq!(major_flats_for(*chrom), *flat);
        }

        for (d_mode, sharp, flat) in [(0, 2, 10), (1, 0, 0), (2, 10, 2), (3, 3, 9), (4, 1, 11), (5, 11, 1), (6, 9, 3)].iter() {
            assert_eq!(sharps_for(2, *d_mode), *sharp);
            assert_eq!(flats_for(2, *d_mode), *flat);
        }
    }
}
