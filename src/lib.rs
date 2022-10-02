use std::cmp::{max, min};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use bare_metal_modulo::{MNum, ModNumC};
use ordered_float::OrderedFloat;
use histogram_macros::*;
use enum_iterator::{all, Sequence};
use float_cmp::{ApproxEq, F64Margin};
use midi_msg::{Channel, ChannelVoiceMsg, MidiMsg};
use midi_msg::MidiMsg::ChannelVoice;
use rand::prelude::SliceRandom;

const MAX_MIDI_VALUE: i16 = i8::MAX as i16;
const NOTES_PER_OCTAVE: i16 = 12;
const USIZE_NOTES_PER_OCTAVE: usize = NOTES_PER_OCTAVE as usize;
const DIATONIC_SCALE_SIZE: usize = 7;
const DIATONIC_SCALE_HOPS: [i16; DIATONIC_SCALE_SIZE] = [2, 2, 1, 2, 2, 2, 1];
const NOTE_NAMES: [&str; NOTES_PER_OCTAVE as usize] = ["C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"];
const MODE_NAMES: [&str; DIATONIC_SCALE_SIZE] = ["ionian", "dorian", "phrygian", "lydian", "mixolydian", "aeolian", "locrian"];

fn assert_prob(p: f64) {
    assert!(0.0 <= p && p <= 1.0);
}

pub fn velocity2volume(midi_velocity: i16) -> f64 {
    max(0, min(MAX_MIDI_VALUE, midi_velocity)) as f64 / MAX_MIDI_VALUE as f64
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Note {
    pitch: i16,
    duration: OrderedFloat<f64>,
    intensity: OrderedFloat<f64>
}

impl ApproxEq for Note {
    type Margin = F64Margin;

    fn approx_eq<M: Into<Self::Margin>>(self, other: Self, margin: M) -> bool {
        let margin = margin.into();
        self.pitch == other.pitch &&
            self.duration.into_inner().approx_eq(other.duration.into_inner(), margin) &&
            self.intensity.into_inner().approx_eq(other.intensity.into_inner(), margin)
    }
}

impl Note {
    pub fn from_midi(pitch: u8, duration: f64, intensity: u8) -> Self {
        Note {
            pitch: pitch as i16,
            duration: OrderedFloat(duration),
            intensity: OrderedFloat(intensity as f64 / MAX_MIDI_VALUE as f64)
        }
    }

    pub fn to_midi(&self) -> (MidiMsg, f64) {
        let note = self.pitch as u8;
        let midi = ChannelVoice {
            channel: Channel::Ch1,
            msg: if self.is_rest() {
                ChannelVoiceMsg::NoteOff { note, velocity: 0 }
            } else {
                ChannelVoiceMsg::NoteOn { note, velocity: (MAX_MIDI_VALUE as f64 * self.intensity.into_inner()) as u8 }
            },
        };
        (midi, self.duration.into_inner())
    }

    pub fn pitch(&self) -> i16 {self.pitch}

    pub fn is_rest(&self) -> bool {self.intensity == OrderedFloat(0.0)}

    pub fn repitched(&self, new_pitch: i16) -> Note {
        Note {pitch: new_pitch, duration: self.duration, intensity: self.intensity}
    }

    pub fn reweigh(&mut self, extra_duration: OrderedFloat<f64>, extra_intensity: OrderedFloat<f64>) {
        let durations = self.duration + extra_duration;
        self.intensity = (self.duration * self.intensity + extra_duration * extra_intensity) / durations;
        self.duration = durations;
    }

    pub fn sonic_pi_list(&self) -> String {
        format!("[{}, {}, {}]", self.pitch, self.duration, self.intensity)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Melody {
    notes: Vec<Note>
}

impl Melody {
    pub fn new() -> Self {Melody {notes: vec![]}}

    pub fn iter(&self) -> impl Iterator<Item=&Note> {
        self.notes.iter()
    }

    pub fn from(s: &str) -> Self {
        let mut notes = Vec::new();
        let mut nums = s.split(",");
        loop {
            match nums.next() {
                Some(note) => {
                    let note = note.parse().unwrap();
                    let duration = nums.next().unwrap().parse().unwrap();
                    let intensity = nums.next().unwrap().parse().unwrap();
                    notes.push(Note { pitch: note, duration, intensity});
                }
                _ => {break;}
            }
        }
        Melody {notes}
    }

    pub fn add(&mut self, n: Note) {
        self.notes.push(n);
    }

    pub fn sonic_pi_list(&self) -> String {
        let mut list_str = self.notes.iter().map(|n| format!("{},", n.sonic_pi_list())).collect::<String>();
        list_str.pop();
        format!("[{}]", list_str)
    }

    pub fn last_note(&self) -> Note {
        *self.notes.last().unwrap()
    }

    pub fn len(&self) -> usize {self.notes.len()}

    pub fn notes_left_from(&self, start: usize) -> usize {
        self.len() - start
    }

    pub fn view_notes(&self) -> String {
        let mut result = String::new();
        for n in self.notes.iter() {
            result.push_str(format!("{} [({:2}) ({:2})] ", NOTE_NAMES[(n.pitch % NOTES_PER_OCTAVE) as usize], n.duration, n.intensity).as_str());
        }
        result
    }

    pub fn without_silence(&self) -> Self {
        Melody {notes: self.notes.iter().filter(|n| n.intensity > OrderedFloat(0.0)).copied().collect()}
    }

    pub fn find_root_pitch(&self) -> i16 {
        let note_iter = self.notes.iter().map(|n| (n.pitch % NOTES_PER_OCTAVE, n.duration));
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

    pub fn get_subdivisions(&self) -> Vec<Self> {
        let no_zeros = self.without_silence();
        let pauses = no_zeros.find_pause_indices();
        no_zeros.subdivide_using(&pauses)
    }

    /// Returns a Vec containing consolidated `Note` objects.
    /// Two or more `Note` objects are consolidated when they
    /// are consecutive in sequence with the same pitch. The `usize`
    /// is the index of the first note in the consecutive subsequence.
    pub fn get_consolidated_notes(&self) -> Vec<(usize,Note)> {
        if self.notes.len() == 0 {
            vec![]
        } else {
            let mut result = vec![(0, self.notes[0])];
            for (i, note) in self.notes.iter().enumerate().skip(1) {
                if result.last().unwrap().1.pitch == note.pitch {
                    result.last_mut().unwrap().1.reweigh(note.duration, note.intensity);
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
            .filter(|i| consolidated[*i-1].1.duration < consolidated[*i].1.duration &&
                consolidated[*i].1.duration > consolidated[*i+1].1.duration)
            .map(|i| consolidated[i].0)
            .collect()
    }

    fn subdivide_using(&self, indices: &Vec<usize>) -> Vec<Self> {
        let mut result = Vec::new();
        let mut current_sub: Vec<Note> = Vec::new();
        let mut current_division = 0;
        for (i, note) in self.notes.iter().enumerate() {
            current_sub.push(*note);
            if current_division < indices.len() && i == indices[current_division] {
                let mut notes = Vec::new();
                std::mem::swap(&mut current_sub, &mut notes);
                result.push(Melody {notes});
                current_division += 1;
            }
        }
        result.push(Melody {notes: current_sub});
        result
    }
}

impl std::ops::Index<usize> for Melody {
    type Output = Note;

    fn index(&self, index: usize) -> &Self::Output {
        &self.notes[index]
    }
}

pub struct MelodyMaker {
    figure_tables: BTreeMap<usize, BTreeMap<i16, Vec<MelodicFigure>>>,
    figure_mappings: HashMap<MelodicFigure,MelodicFigure>
}

impl MelodyMaker {
    pub fn new() -> Self {
        MelodyMaker {
            figure_tables: (3..=4).map(|len| (len, MelodicFigure::interval2figures(len))).collect(),
            figure_mappings: HashMap::new()
        }
    }

    /// Finds a MelodicFigure that matches the pitch sequence in `melody` starting at `start`.
    /// Prefers matching a 4-note figure to a 3-note figure.
    pub fn matching_figure(&self, melody: &Melody, start: usize) -> Option<MelodicFigure> {
        let scale = melody.best_scale_for();
        let space_left = melody.notes_left_from(start);
        for length in [4, 3] {
            if space_left >= length {
                if let Some(step_gap) = scale.diatonic_steps_between(melody[start].pitch, melody[start + length - 1].pitch) {
                    if let Some(candidates) = self.figure_tables.get(&length).unwrap().get(&step_gap) {
                        for candidate in candidates.iter() {
                            if candidate.matches(melody, &scale, start) {
                                return Some(*candidate);
                            }
                        }
                    }
                }
            }
        }
        None
    }

    pub fn is_chain(chain: &VecDeque<(usize,Option<MelodicFigure>)>) -> bool {
        let mut current = 0;
        for (i,f) in chain.iter() {
            if *i != current {
                return false;
            } else {
                current += f.map_or(1, |f| f.len() - 1);
            }
        }
        true
    }

    pub fn greedy_figure_chain(&self, melody: &Melody) -> VecDeque<(usize,Option<MelodicFigure>)> {
        let mut start = 0;
        let mut figures = VecDeque::new();
        while start < melody.len() {
            if let Some(matched) = self.matching_figure(melody, start) {
                figures.push_back((start, Some(matched)));
                start += matched.len() - 1;
            } else {
                figures.push_back((start, None));
                start += 1;
            }
        }
        figures
    }

    pub fn all_figure_matches(&self, melody: &Melody) -> Vec<(usize,MelodicFigure)> {
        (0..melody.len())
            .filter_map(|i| self.matching_figure(melody, i)
                .map(|f| (i, f)))
            .collect()
    }

    pub fn emphasis_figure_chain(&self, melody: &Melody, emphasized_indices: &Vec<usize>) -> VecDeque<(usize,Option<MelodicFigure>)> {
        let all_matched = self.all_figure_matches(melody);
        let emphasized_indices: BTreeSet<usize> = emphasized_indices.iter().copied().collect();
        let mut keepers = BTreeMap::new();
        self.add_if_clear(&all_matched, &mut keepers, |s, f| emphasized_indices.contains(&(s + f.len() - 1)));
        self.add_if_clear(&all_matched, &mut keepers, |s, _f| emphasized_indices.contains(&s));
        self.add_if_clear(&all_matched, &mut keepers, |_s, _f| true);
        Self::keepers2chain(&keepers, melody)
    }

    pub fn locked_in_figures(&self, melody: &Melody) -> VecDeque<(usize,Option<MelodicFigure>)> {
        let all_matched = self.all_figure_matches(melody);
        let figure2count = collect_from_into!(all_matched.iter().copied().map(|(_, f)| f), HashMap::<MelodicFigure,usize>::new());
        let mut ranked = ranking!(figure2count);
        let mut rng = rand::thread_rng();
        let mut keepers = BTreeMap::new();
        while ranked.len() > 0 {
            let max_count = ranked[0].1;
            let mut candidates = HashSet::new();
            while ranked.len() > 0 && ranked[0].1 == max_count {
                candidates.insert(ranked.pop_front().unwrap().0);
            }
            let mut candidates = candidates.iter().copied().collect::<Vec<_>>();
            candidates.shuffle(&mut rng);
            while candidates.len() > 0 {
                let figure = candidates.pop().unwrap();
                for start in all_matched.iter().filter(|(_,f)| *f == figure).map(|(i, _)| *i) {
                    self.add_no_interference(&mut keepers, figure, start);
                }
            }
        }
        Self::keepers2chain(&keepers, melody)
    }

    fn keepers2chain(keepers: &BTreeMap<usize,MelodicFigure>, melody: &Melody) -> VecDeque<(usize,Option<MelodicFigure>)> {
        let mut covered = HashSet::new();
        let mut result = VecDeque::new();
        for i in 0..melody.len() {
            if !covered.contains(&i) {
                result.push_back((i, match keepers.get(&i) {
                    None => {covered.insert(i); None}
                    Some(f) => {
                        for c in 0..(f.len() - 1) {
                            covered.insert(i + c);
                        }
                        Some(*f)
                    }
                }));
            }
        }
        result
    }

    fn add_if_clear<P:Fn(usize,MelodicFigure)->bool>(&self, all_matched: &Vec<(usize, MelodicFigure)>, keepers: &mut BTreeMap<usize,MelodicFigure>, filter: P) {
        for (start, figure) in all_matched.iter() {
            if filter(*start, *figure) {
                self.add_no_interference(keepers, *figure, *start);
            }
        }
    }

    fn add_no_interference(&self, keepers: &mut BTreeMap<usize,MelodicFigure>, figure: MelodicFigure, start: usize) {
        let prev_start = max(start as isize - *self.figure_tables.keys().max().unwrap() as isize, 0) as usize;
        let mut prev_zone = prev_start..(start + figure.len());
        if !prev_zone.any(|i| keepers.get(&i).map_or(false, |f| figure.interfere(start, *f, i))) {
            keepers.insert(start, figure);
        }
    }

    pub fn create_variation_1(&mut self, original: &Melody, p_rewrite: f64) -> Melody {
        self.chain_variation_creator(original, p_rewrite, |s, m| s.greedy_figure_chain(m), Self::pick_figure)
    }

    fn chain_variation_creator<C,P>(&mut self, original: &Melody, p_rewrite: f64, chain_maker: C, mut figure_picker: P) -> Melody
        where C:Fn(&Self,&Melody)->VecDeque<(usize,Option<MelodicFigure>)>,
              P:FnMut(&mut Self,MelodicFigure)->MelodicFigure {
        assert_prob(p_rewrite);
        let original = original.without_silence();
        let scale = original.best_scale_for();
        let mut figure_chain = chain_maker(self, &original);
        assert!(Self::is_chain(&figure_chain));
        let mut notes = vec![];
        let mut next_already_added = false;
        loop {
            match figure_chain.pop_front() {
                None => {break;},
                Some((i, figure)) => {
                    if !next_already_added {
                        notes.push(original[i]);
                    }
                    match figure {
                        None => {next_already_added = false;}
                        Some(figure) => {
                            let generator = if rand::random::<f64>() < p_rewrite {
                                figure_picker(self, figure)
                            } else {figure};
                            for (offset, pitch) in generator.make_pitches(notes.last().unwrap().pitch, &scale).iter().enumerate().skip(1) {
                                notes.push(original[i + offset].repitched(*pitch));
                            }
                            next_already_added = true;
                        }
                    }
                }
            }
        }
        Melody {notes}
    }

    fn find_jump<P:Fn(&i16)->bool>(scale: &MusicMode, start_pitch: i16, notes_to_use: &VecDeque<Note>, end_jump_index: usize, filter: P) -> Option<i16> {
        notes_to_use.get(end_jump_index)
            .and_then(|n| scale.diatonic_steps_between(n.pitch, start_pitch))
            .filter(|p| p.abs() < 8 && filter(p))
    }

    pub fn pick_figure(&mut self, figure: MelodicFigure) -> MelodicFigure {
        let figure_length = figure.len();
        let jump = figure.total_change();
        let table = self.figure_tables.get(&figure_length).unwrap();
        let options = table.get(&jump).unwrap().iter()
            .filter(|f| **f != figure)
            .copied().collect::<Vec<_>>();
        if options.len() > 0 {options[rand::random::<usize>() % options.len()]} else {figure}
    }

    pub fn create_variation_2(&mut self, original: &Melody, p_rewrite: f64) -> Melody {
        self.chain_variation_creator(original, p_rewrite, |s, m| s.emphasis_figure_chain(m, &m.find_pause_indices()), Self::pick_figure)
    }

    pub fn create_variation_4(&mut self, original: &Melody, p_remap: f64) -> Melody {
        assert_prob(p_remap);
        self.reset_figure_mappings();
        self.chain_variation_creator(original, 1.0, |s, m| s.locked_in_figures(m),
                                     |s, f| s.pick_remembered_figure(f, p_remap))
    }

    fn pick_remembered_figure(&mut self, figure: MelodicFigure, p_remap: f64) -> MelodicFigure {
        match self.figure_mappings.get(&figure) {
            None => {
                let r = if rand::random::<f64>() < p_remap {self.pick_figure(figure)} else {figure};
                self.figure_mappings.insert(figure, r);
                r
            }
            Some(r) => {*r}
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
    octave_notes: [ModNumC<i16,USIZE_NOTES_PER_OCTAVE>; DIATONIC_SCALE_SIZE]
}

impl MusicMode {
    pub fn all_modes_for(root_note: i16) -> Vec<Self> {
        (0..DIATONIC_SCALE_SIZE)
            .map(|i| Self::new(ModNumC::new(i), root_note))
            .collect()
    }

    pub fn name(&self) -> String {
        format!("{} {}", NOTE_NAMES[self.root() as usize], MODE_NAMES[self.root_pos.a()])
    }

    pub fn new(root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>, root_note: i16) -> Self {
        let mut octave_notes = [ModNumC::new(root_note); DIATONIC_SCALE_SIZE];
        let mut offset = DIATONIC_SCALE_HOPS[root_pos.a()];
        for i in root_pos.iter().skip(1) {
            octave_notes[i.a()] += offset;
            offset += DIATONIC_SCALE_HOPS[i.a()];
        }
        MusicMode {root_pos, octave_notes}
    }

    pub fn diatonic_steps_between(&self, pitch1: i16, pitch2: i16) -> Option<i16> {
        assert!(pitch1 < i8::MAX as i16 + NOTES_PER_OCTAVE);
        if pitch1 > pitch2 {
            self.diatonic_steps_between(pitch2, pitch1).map(|steps| -steps)
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
    pub fn next_pitch(&self, reference_pitch: i16, scale_steps_away: i16) -> i16 {
        assert!(reference_pitch < i16::MAX);
        let mut octaves_up = reference_pitch / NOTES_PER_OCTAVE;
        self.octave_notes.iter()
            .position(|p| *p == reference_pitch)
            .map(|i| {
                let ref_octave_basis = self.octave_notes[i].a();
                let j: ModNumC<i16, DIATONIC_SCALE_SIZE> = ModNumC::new(i as i16 + scale_steps_away);
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

    fn root(&self) -> i16 {
        self.octave_notes[self.root_pos.a()].a()
    }

    pub fn contains(&self, pitch: i16) -> bool {
        self.octave_notes.contains(&(ModNumC::new(pitch)))
    }

    pub fn diatonic_degree(&self, pitch: i16) -> Option<i16> {
        let mut pertinent_root = self.root();
        while pertinent_root + NOTES_PER_OCTAVE <= pitch {
            pertinent_root += NOTES_PER_OCTAVE;
        }
        self.diatonic_steps_between(pertinent_root, pitch).map(|d| d + 1)
    }
}

// Inspired by: https://figuringoutmelody.com/the-24-universal-melodic-figures/
#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence, Hash, Ord, PartialOrd)]
pub struct MelodicFigure {
    shape: MelodicFigureShape, polarity: FigurePolarity, direction: FigureDirection
}

impl MelodicFigure {
    pub fn pattern(&self) -> Vec<i16> {
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

    pub fn len(&self) -> usize {self.pattern().len() + 1}

    /// Returns the net change of diatonic steps from the start to the end of this `MelodicFigure`.
    pub fn total_change(&self) -> i16 {self.pattern().iter().sum()}

    /// Two `MelodicFigure` objects `interfere()` if they overlap anywhere except at their endpoints.
    pub fn interfere(&self, self_start: usize, other: MelodicFigure, other_start: usize) -> bool {
        let mut self_range = self_start..(self_start + self.len());
        let other_range = (other_start + 1)..(other_start + other.len() - 1);
        self_range.any(|i| other_range.contains(&i))
    }

    pub fn matches(&self, melody: &Melody, scale: &MusicMode, start: usize) -> bool {
        let mut p = 0;
        let mut m = start;
        while p < self.pattern().len() && m + 1 < melody.len() {
            if melody[m] != melody[m + 1] {
                match scale.diatonic_steps_between(melody[m].pitch, melody[m + 1].pitch) {
                    None => return false,
                    Some(actual) => if actual != self.pattern()[p] {return false;}
                }
                p += 1;
            }
            m += 1;
        }
        true
    }

    /// Generates a sequence of diatonic pitches derived from `scale` corresponding to
    /// `self.pattern()`. The sequence starts at `start_pitch` (which is included in the result).
    pub fn make_pitches(&self, start_pitch: i16, scale: &MusicMode) -> Vec<i16> {
        let mut result = vec![start_pitch];
        for interval in self.pattern() {
            result.push(scale.next_pitch(*result.last().unwrap(), interval));
        }
        result
    }

    pub fn interval2figures(figure_length: usize) -> BTreeMap<i16, Vec<Self>> {
        let mut result = BTreeMap::new();
        for m in all::<MelodicFigure>() {
            if m.len() == figure_length {
                let interval = m.total_change();
                match result.get_mut(&interval) {
                    None => { result.insert(interval, vec![m]); }
                    Some(v) => { v.push(m); }
                }
            }
        }
        result
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence, Hash, Ord, PartialOrd)]
pub enum FigurePolarity {
    Positive, Negative
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence, Hash, Ord, PartialOrd)]
pub enum FigureDirection {
    Forward, Reverse
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence, Hash, Ord, PartialOrd)]
pub enum MelodicFigureShape {
    Note3Scale, Auxiliary, Arpeggio, Run, Trill1, Trill2, Arch, NP3, PivotLHP, ReturnCrazyDriver,
    ArpeggioPlus, Parkour1, ParkourBounce2, ParkourPounce2, Vault4, Vault5, Vault6, Vault7, Roll,
    DoubleNeighbor, Double3rd, Pendulum43, Pendulum54, LeapingScale, LeapingAux1, LeapingAux2,
    PendulumAux1, PendulumAux2, Funnel, Cambiata1, Cambiata2, ZigZag1, ZigZag2, MysteryCountdown
}

impl MelodicFigureShape {
    pub fn pattern(&self) -> Vec<i16> {
        match self {
            MelodicFigureShape::Note3Scale        => vec![1, 1],
            MelodicFigureShape::Auxiliary         => vec![-1, 1],
            MelodicFigureShape::Arpeggio          => vec![2, 2],
            MelodicFigureShape::Run               => vec![1, 1, 1],
            MelodicFigureShape::Trill1            => vec![1, -1, 1],
            MelodicFigureShape::Trill2            => vec![2, -2, 2],
            MelodicFigureShape::Arch              => vec![2, 2, -2],
            MelodicFigureShape::NP3               => vec![-2, -1],
            MelodicFigureShape::PivotLHP          => vec![1, -2],
            MelodicFigureShape::ReturnCrazyDriver => vec![1, 1, -1],
            MelodicFigureShape::ArpeggioPlus      => vec![2, 2, -1],
            MelodicFigureShape::Parkour1          => vec![-1, 3],
            MelodicFigureShape::ParkourPounce2    => vec![1, -6],
            MelodicFigureShape::ParkourBounce2    => vec![-5, 1],
            MelodicFigureShape::Vault4            => vec![4, 1],
            MelodicFigureShape::Vault5            => vec![5, 1],
            MelodicFigureShape::Vault6            => vec![6, 1],
            MelodicFigureShape::Vault7            => vec![1, 7],
            MelodicFigureShape::Roll              => vec![1, 1, -2],
            MelodicFigureShape::DoubleNeighbor    => vec![1, -2, 1],
            MelodicFigureShape::Double3rd         => vec![2, -1, 2],
            MelodicFigureShape::Pendulum43        => vec![4, -3],
            MelodicFigureShape::Pendulum54        => vec![5, -4],
            MelodicFigureShape::LeapingScale      => vec![1, 1, 2],
            MelodicFigureShape::LeapingAux1       => vec![-1, 1, 4],
            MelodicFigureShape::LeapingAux2       => vec![1, -1, 4],
            MelodicFigureShape::PendulumAux1      => vec![4, -5, 1],
            MelodicFigureShape::PendulumAux2      => vec![4, -3, -1],
            MelodicFigureShape::Funnel            => vec![3, -2, 1],
            MelodicFigureShape::Cambiata1         => vec![1, 2, -1],
            MelodicFigureShape::Cambiata2         => vec![1, -4, 5],
            MelodicFigureShape::ZigZag1           => vec![4, -2, 5],
            MelodicFigureShape::ZigZag2           => vec![-1, 5, -1],
            MelodicFigureShape::MysteryCountdown  => vec![1, -5, 3]
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cmp::{max, min};
    use std::collections::{BTreeSet, VecDeque};
    use bare_metal_modulo::ModNumC;
    use ordered_float::OrderedFloat;
    use crate::{DIATONIC_SCALE_SIZE, MelodicFigure, Melody, MelodyMaker, MusicMode};
    use crate::MelodicFigureShape::*;
    use crate::FigurePolarity::*;
    use crate::FigureDirection::*;
    use float_cmp::assert_approx_eq;

    const EXAMPLE_MELODY: &str = "55,0.39,0.91,55,0.04,0.0,59,0.33,0.73,60,0.06,0.44,62,0.02,0.87,59,0.05,0.0,60,0.16,0.0,62,0.2,0.0,55,0.39,0.61,55,0.01,0.0,57,0.34,0.98,57,0.05,0.0,55,0.39,0.78,54,0.02,0.98,55,0.19,0.0,54,0.12,0.0,52,0.11,0.74,52,0.0,0.0,54,0.12,0.46,54,0.03,0.0,50,0.1,0.84,50,0.27,0.0,55,0.27,0.74,55,0.1,0.0,59,0.27,0.44,60,0.07,0.54,62,0.04,0.91,59,0.09,0.0,60,0.11,0.0,62,0.19,0.0,55,0.29,0.67,55,0.07,0.0,57,0.32,0.76,57,0.06,0.0,55,0.23,0.7,55,0.05,0.0,54,0.12,0.93,54,0.07,0.0,50,0.37,0.8,50,0.5,0.0,55,0.36,0.76,55,0.05,0.0,59,0.28,0.76,60,0.05,0.7,62,0.01,0.91,59,0.07,0.0,60,0.15,0.0,62,0.2,0.0,55,0.33,0.67,55,0.02,0.0,57,0.29,0.8,57,0.1,0.0,55,0.29,0.9,55,0.08,0.0,54,0.16,1.0,54,0.12,0.0,52,0.12,0.72,54,0.01,0.71,52,0.14,0.0,54,0.07,0.0,50,0.1,0.76,50,0.23,0.0,55,0.22,0.65,55,0.13,0.0,57,0.29,0.64,57,0.08,0.0,55,0.23,0.76,55,0.07,0.0,54,0.12,0.99,54,0.04,0.0,52,0.24,0.95,52,0.19,0.0,54,0.13,1.0,54,0.15,0.0,52,0.12,0.72,52,0.03,0.0,54,0.19,0.83,54,0.13,0.0,50,0.06,0.69,50,0.15,0.0,55,0.01,0.73,57,0.07,0.66,57,0.55,0.0,55,1.5,0.0";
    const COUNTDOWN_MELODY: &str = "66,0.42,1.0,66,0.55,0.0,73,0.17,1.0,73,0.01,0.0,71,0.13,0.77,71,0.0,0.0,73,0.45,0.41,73,0.13,0.0,66,0.85,0.8,66,0.32,0.0,74,0.16,1.0,74,0.0,0.0,74,0.37,0.87,74,0.03,0.0,73,0.2,1.0,73,0.03,0.0,71,0.03,0.06,71,0.04,0.0,71,0.93,1.0,71,0.27,0.0,74,0.16,1.0,74,0.03,0.0,73,0.13,1.0,73,0.03,0.0,74,0.45,1.0,74,0.12,0.0,66,0.58,0.8,66,0.5,0.0,71,0.15,0.75,71,0.02,0.0,71,0.13,0.81,71,0.03,0.0,71,0.21,1.0,71,0.08,0.0,69,0.24,0.94,69,0.08,0.0,68,0.22,0.65,68,0.07,0.0,71,0.24,1.0,71,0.06,0.0,69,0.68,1.0,69,0.15,0.0,73,0.16,1.0,73,0.03,0.0,71,0.14,0.91,71,0.03,0.0,73,0.29,1.0,73,0.22,0.0,66,0.61,0.64,66,0.45,0.0,74,0.15,0.87,74,0.04,0.0,74,0.14,0.83,74,0.02,0.0,74,0.2,1.0,74,0.13,0.0,73,0.29,0.96,73,0.0,0.0,72,0.04,0.49,72,0.03,0.0,71,1.01,1.0,71,0.41,0.0,74,0.14,0.94,74,0.04,0.0,73,0.13,0.8,73,0.03,0.0,74,0.49,1.0,74,0.12,0.0,66,0.93,0.54,66,0.19,0.0,71,0.16,0.81,71,0.02,0.0,71,0.13,0.79,71,0.03,0.0,71,0.21,0.87,71,0.11,0.0,69,0.24,0.86,69,0.08,0.0,68,0.24,0.67,68,0.07,0.0,71,0.24,1.0,71,0.11,0.0,69,0.75,0.86,69,0.05,0.0,68,0.18,0.71,68,0.02,0.0,69,0.16,0.89,69,0.04,0.0,71,0.02,0.99,71,0.0,0.0,83,0.01,1.0,83,0.0,0.0,71,0.56,0.98,71,0.16,0.0,69,0.19,1.0,69,0.04,0.0,71,0.2,1.0,71,0.05,0.0,73,0.24,1.0,73,0.0,0.0,72,0.03,0.62,72,0.07,0.0,71,0.2,0.91,71,0.03,0.0,69,0.01,0.06,69,0.06,0.0,69,0.18,0.73,69,0.11,0.0,68,0.19,0.46,68,0.18,0.0,66,0.51,0.76,66,0.17,0.0,74,0.56,1.0,74,0.01,0.0,73,1.09,0.79,73,0.07,0.0,75,0.16,0.9,75,0.03,0.0,73,0.16,0.84,73,0.03,0.0,71,0.18,0.57,71,0.03,0.0,73,0.78,0.64,73,0.06,0.0,73,0.14,0.91,73,0.04,0.0,73,0.14,0.87,73,0.04,0.0,73,0.26,0.81,73,0.1,0.0,71,0.23,0.91,71,0.07,0.0,69,0.19,0.98,69,0.1,0.0,68,0.23,0.59,68,0.15,0.0,66,1.22,0.68,66,2.0,0.0";

    #[test]
    fn test_parse_melody() {
        let m = "69,0.24,1.0,69,0.09,0.0,72,0.31,1.0,72,0.08,0.0,71,0.29,0.69";
        let notes = Melody::from(m);
        println!("{}", notes.view_notes());
        assert_eq!(format!("{:?}", notes), "Melody { notes: [Note { pitch: 69, duration: OrderedFloat(0.24), intensity: OrderedFloat(1.0) }, Note { pitch: 69, duration: OrderedFloat(0.09), intensity: OrderedFloat(0.0) }, Note { pitch: 72, duration: OrderedFloat(0.31), intensity: OrderedFloat(1.0) }, Note { pitch: 72, duration: OrderedFloat(0.08), intensity: OrderedFloat(0.0) }, Note { pitch: 71, duration: OrderedFloat(0.29), intensity: OrderedFloat(0.69) }] }");
    }

    fn test_natural_mode(root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>, notes: [i16; 15]) {
        let mode = MusicMode::new(root_pos, notes[0]);
        println!("{} {:?}", mode.name(), mode);
        for (i, n) in notes.iter().enumerate() {
            let i = i as i16;
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

        let not_c_notes: [i16; 5] = [73, 75, 78, 80, 82];
        for n in not_c_notes.iter() {
            assert!(!mode.contains(*n));
        }
    }

    #[test]
    fn test_scales() {
        test_natural_mode(ModNumC::new(0), [72, 74, 76, 77, 79, 81, 83, 84, 86, 88, 89, 91, 93, 95, 96]);
    }

    #[test]
    fn test_scales_dorian() {
        test_natural_mode(ModNumC::new(1), [74, 76, 77, 79, 81, 83, 84, 86, 88, 89, 91, 93, 95, 96, 98]);
    }

    #[test]
    fn test_modes() {
        let modes = MusicMode::all_modes_for(72);
        let expected: [[i16; DIATONIC_SCALE_SIZE]; DIATONIC_SCALE_SIZE] = [
            [0, 2, 4, 5, 7, 9, 11],
            [10, 0, 2, 3, 5, 7, 9],
            [8, 10, 0, 1, 3, 5, 7],
            [7, 9, 11, 0, 2, 4, 6],
            [5, 7, 9, 10, 0, 2, 4],
            [3, 5, 7, 8, 10, 0, 2],
            [1, 3, 5, 6, 8, 10, 0]
        ];
        for i in 0..DIATONIC_SCALE_SIZE {
            assert_eq!(modes[i].octave_notes, expected[i]);
        }
    }

    #[test]
    fn test_interval_table() {
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
        let tune = Melody::from(EXAMPLE_MELODY).without_silence();
        let scale = tune.best_scale_for();
        let mut maker = MelodyMaker::new();
        for _ in 0..20 {
            let var = maker.create_variation_1(&tune, 0.9);
            assert_eq!(var.len(), tune.len());
            let mut different_count = 0;
            for i in 0..var.len() {
                assert_eq!(var[i].duration, tune[i].duration);
                assert_eq!(var[i].intensity, tune[i].intensity);
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
        let mode = MusicMode { root_pos: ModNumC::new(0), octave_notes: [ModNumC::new(7), ModNumC::new(9), ModNumC::new(11), ModNumC::new(0), ModNumC::new(2), ModNumC::new(4), ModNumC::new(6) ] };
        let steps = mode.diatonic_steps_between(54, 57);
        assert_eq!(steps.unwrap(), 2);
    }

    #[test]
    fn test_next_note_bug() {
        let mode = MusicMode { root_pos: ModNumC::new(0), octave_notes: [ModNumC::new(7), ModNumC::new(9), ModNumC::new(11), ModNumC::new(0), ModNumC::new(2), ModNumC::new(4), ModNumC::new(6) ] };
        println!("mode: {}", mode.name());
        println!("{:?}", mode);
        let tests = [
            (54, 1, 55), (67, 1, 69), (69, 1, 71), (71, 1, 72), (60, 1, 62), (62, 1, 64),
            (64, 1, 66), (66, 1, 67), (79, 1, 81), (81, 1, 83), (83, 1, 84), (72, 1, 74),
            (74, 1, 76), (76, 1, 78), (78, 1, 79), (91, 1, 93), (93, 1, 95), (95, 1, 96),
            (84, 1, 86), (86, 1, 88), (88, 1, 90), (90, 1, 91), (103, 1, 105), (105, 1, 107),
            (107, 1, 108), (96, 1, 98), (98, 1, 100), (100, 1, 102), (102, 1, 103), (115, 1, 117),
            (117, 1, 119), (108, 1, 110), (110, 1, 112), (112, 1, 114), (114, 1, 115),
            (120, 1, 122), (122, 1, 124)
        ];
        for (reference_pitch, scale_steps_away, expected) in tests {
            let np = mode.next_pitch(reference_pitch, scale_steps_away);
            assert_eq!(np, expected);
        }
    }

    #[test]
    fn test_send_back() {
        let tune = Melody::from(EXAMPLE_MELODY).without_silence();
        assert_eq!(tune.sonic_pi_list(), "[[55, 0.39, 0.91],[59, 0.33, 0.73],[60, 0.06, 0.44],[62, 0.02, 0.87],[55, 0.39, 0.61],[57, 0.34, 0.98],[55, 0.39, 0.78],[54, 0.02, 0.98],[52, 0.11, 0.74],[54, 0.12, 0.46],[50, 0.1, 0.84],[55, 0.27, 0.74],[59, 0.27, 0.44],[60, 0.07, 0.54],[62, 0.04, 0.91],[55, 0.29, 0.67],[57, 0.32, 0.76],[55, 0.23, 0.7],[54, 0.12, 0.93],[50, 0.37, 0.8],[55, 0.36, 0.76],[59, 0.28, 0.76],[60, 0.05, 0.7],[62, 0.01, 0.91],[55, 0.33, 0.67],[57, 0.29, 0.8],[55, 0.29, 0.9],[54, 0.16, 1],[52, 0.12, 0.72],[54, 0.01, 0.71],[50, 0.1, 0.76],[55, 0.22, 0.65],[57, 0.29, 0.64],[55, 0.23, 0.76],[54, 0.12, 0.99],[52, 0.24, 0.95],[54, 0.13, 1],[52, 0.12, 0.72],[54, 0.19, 0.83],[50, 0.06, 0.69],[55, 0.01, 0.73],[57, 0.07, 0.66]]");
    }

    #[test]
    fn test_subdivide_melody() {
        let expected_subs = [
            "55,0.39,0.91,55,0.04,0.0,59,0.33,0.73,60,0.06,0.44,62,0.02,0.87,59,0.05,0.0,60,0.16,0.0,62,0.2,0.0,55,0.39,0.61",
            "55,0.01,0.0,57,0.34,0.98,57,0.05,0.0,55,0.39,0.78",
            "54,0.02,0.98,55,0.19,0.0,54,0.12,0.0,52,0.11,0.74,52,0.0,0.0,54,0.12,0.46",
            "54,0.03,0.0,50,0.1,0.84,50,0.27,0.0,55,0.27,0.74,55,0.1,0.0,59,0.27,0.44,60,0.07,0.54,62,0.04,0.91,59,0.09,0.0,60,0.11,0.0,62,0.19,0.0,55,0.29,0.67,55,0.07,0.0,57,0.32,0.76",
            "57,0.06,0.0,55,0.23,0.7,55,0.05,0.0,54,0.12,0.93,54,0.07,0.0,50,0.37,0.8",
            "50,0.5,0.0,55,0.36,0.76,55,0.05,0.0,59,0.28,0.76,60,0.05,0.7,62,0.01,0.91,59,0.07,0.0,60,0.15,0.0,62,0.2,0.0,55,0.33,0.67",
            "55,0.02,0.0,57,0.29,0.8,57,0.1,0.0,55,0.29,0.9,55,0.08,0.0,54,0.16,1.0,54,0.12,0.0,52,0.12,0.72,54,0.01,0.71,52,0.14,0.0,54,0.07,0.0,50,0.1,0.76,50,0.23,0.0,55,0.22,0.65,55,0.13,0.0,57,0.29,0.64",
            "57,0.08,0.0,55,0.23,0.76,55,0.07,0.0,54,0.12,0.99,54,0.04,0.0,52,0.24,0.95",
            "52,0.19,0.0,54,0.13,1.0,54,0.15,0.0,52,0.12,0.72,52,0.03,0.0,54,0.19,0.83",
            "54,0.13,0.0,50,0.06,0.69,50,0.15,0.0,55,0.01,0.73,57,0.07,0.66,57,0.55,0.0,55,1.5,0.0"
        ];

        let notes = Melody::from(EXAMPLE_MELODY);
        println!("{:?}", notes);
        let subs = notes.get_subdivisions();
        assert_eq!(subs.len(), expected_subs.len());
        for i in 0..expected_subs.len() {
            println!("sub length: {} {}", subs[i].len(), subs[i].view_notes());
            assert_eq!(Melody::from(expected_subs[i]).without_silence(), subs[i]);
        }

        let best = notes.best_scale_for();
        println!("{} {:?}", best.name(), best);
    }

    #[test]
    fn test_diatonic_degree() {
        let melody = Melody::from(EXAMPLE_MELODY).without_silence();
        let scale = melody.best_scale_for();
        assert_eq!(scale.name(), "G ionian");
        for (i, pitch) in [67, 69, 71, 72, 74, 76, 78, 79, 81, 83, 84, 86, 88, 90, 91].iter().enumerate() {
            assert_eq!((i % DIATONIC_SCALE_SIZE) as i16 + 1, scale.diatonic_degree(*pitch).unwrap());
        }
    }

    #[test]
    fn test_figure_match_1() {
        let (figure_count, figure_set) = test_figure_match(EXAMPLE_MELODY);
        assert_eq!(figure_count, 29);
        assert_eq!(figure_set.len(), 12);
        assert_eq!(format!("{:?}", figure_set), "{MelodicFigure { shape: Note3Scale, polarity: Positive, direction: Forward }, MelodicFigure { shape: Auxiliary, polarity: Positive, direction: Forward }, MelodicFigure { shape: Run, polarity: Negative, direction: Forward }, MelodicFigure { shape: Trill1, polarity: Positive, direction: Forward }, MelodicFigure { shape: Trill1, polarity: Negative, direction: Forward }, MelodicFigure { shape: NP3, polarity: Positive, direction: Reverse }, MelodicFigure { shape: ReturnCrazyDriver, polarity: Negative, direction: Forward }, MelodicFigure { shape: ReturnCrazyDriver, polarity: Negative, direction: Reverse }, MelodicFigure { shape: LeapingScale, polarity: Positive, direction: Reverse }, MelodicFigure { shape: LeapingScale, polarity: Negative, direction: Forward }, MelodicFigure { shape: LeapingAux2, polarity: Negative, direction: Reverse }, MelodicFigure { shape: Funnel, polarity: Positive, direction: Reverse }}");    }

    #[test]
    fn test_figure_match_countdown() {
        let (figure_count, figure_set) = test_figure_match(COUNTDOWN_MELODY);
        assert_eq!(figure_count, 36);
        assert_eq!(figure_set.len(), 20);
        assert_eq!(format!("{:?}", figure_set), "{MelodicFigure { shape: Note3Scale, polarity: Positive, direction: Forward }, MelodicFigure { shape: Note3Scale, polarity: Negative, direction: Forward }, MelodicFigure { shape: Auxiliary, polarity: Positive, direction: Forward }, MelodicFigure { shape: Run, polarity: Negative, direction: Forward }, MelodicFigure { shape: PivotLHP, polarity: Negative, direction: Reverse }, MelodicFigure { shape: ReturnCrazyDriver, polarity: Positive, direction: Reverse }, MelodicFigure { shape: ReturnCrazyDriver, polarity: Negative, direction: Forward }, MelodicFigure { shape: ParkourBounce2, polarity: Negative, direction: Forward }, MelodicFigure { shape: Vault7, polarity: Positive, direction: Forward }, MelodicFigure { shape: Vault7, polarity: Negative, direction: Reverse }, MelodicFigure { shape: Roll, polarity: Negative, direction: Forward }, MelodicFigure { shape: Roll, polarity: Negative, direction: Reverse }, MelodicFigure { shape: DoubleNeighbor, polarity: Negative, direction: Forward }, MelodicFigure { shape: Double3rd, polarity: Positive, direction: Forward }, MelodicFigure { shape: Pendulum54, polarity: Positive, direction: Reverse }, MelodicFigure { shape: LeapingAux2, polarity: Positive, direction: Reverse }, MelodicFigure { shape: LeapingAux2, polarity: Negative, direction: Forward }, MelodicFigure { shape: Cambiata2, polarity: Positive, direction: Forward }, MelodicFigure { shape: ZigZag2, polarity: Positive, direction: Forward }, MelodicFigure { shape: MysteryCountdown, polarity: Positive, direction: Forward }}");
    }

    fn test_figure_match(melody_str: &str) -> (usize, BTreeSet<MelodicFigure>) {
        let melody = Melody::from(melody_str).without_silence();
        let scale = melody.best_scale_for();
        println!("scale: {}", scale.name());
        let maker = MelodyMaker::new();
        let mut figure_count = 0;
        let mut figure_set = BTreeSet::new();
        for i in 0..melody.len() {
            if let Some(matched) = maker.matching_figure(&melody, i) {
                figure_count += 1;
                figure_set.insert(matched);
                let pitches = matched.make_pitches(melody[i].pitch, &scale);
                let melody_pitches = (i..(i + pitches.len())).map(|i| melody[i].pitch).collect::<Vec<_>>();
                assert_eq!(pitches, melody_pitches);
                println!("start: {}\t{:?}: figure: {:?} first: {:?} last: {:?}",
                         i, matched, pitches,
                         scale.diatonic_degree(pitches[0]),
                         scale.diatonic_degree(*pitches.last().unwrap()));
            } else {
                if i > 0 {
                    println!("start: {}\tpitch: {}\tsteps to: {:?}", i, melody[i].pitch, scale.diatonic_steps_between(melody[i-1].pitch, melody[i].pitch));
                } else {
                    println!("start: {}\tpitch: {}", i, melody[i].pitch);
                }
            }
        }
        println!("# figures: {}", figure_count);
        println!("# distinct figures: {}", figure_set.len());
        println!("figures: {:?}", figure_set);
        println!("pause indices: {:?}", melody.find_pause_indices());
        (figure_count, figure_set)
    }

    #[test]
    fn test_emphasized_countdown() {
        let expected = vec![
            (0, None),
            (1, Some(MelodicFigure { shape: LeapingAux2, polarity: Negative, direction: Forward })),
            (4, None), (5, None),
            (6, Some(MelodicFigure { shape: Note3Scale, polarity: Negative, direction: Forward })),
            (8, None),
            (9, Some(MelodicFigure { shape: PivotLHP, polarity: Negative, direction: Reverse })),
            (11, Some(MelodicFigure { shape: MysteryCountdown, polarity: Positive, direction: Forward })),
            (14, None), (15, None), (16, None),
            (17, Some(MelodicFigure { shape: DoubleNeighbor, polarity: Negative, direction: Forward })),
            (20, None),
            (21, Some(MelodicFigure { shape: LeapingAux2, polarity: Negative, direction: Forward })),
            (24, None), (25, None), (26, None), (27, None), (28, None), (29, None),
            (30, Some(MelodicFigure { shape: PivotLHP, polarity: Negative, direction: Reverse })),
            (32, Some(MelodicFigure { shape: MysteryCountdown, polarity: Positive, direction: Forward })),
            (35, None), (36, None), (37, None),
            (38, Some(MelodicFigure { shape: DoubleNeighbor, polarity: Negative, direction: Forward })),
            (41, Some(MelodicFigure { shape: ReturnCrazyDriver, polarity: Positive, direction: Reverse })),
            (44, None), (45, None),
            (46, Some(MelodicFigure { shape: ReturnCrazyDriver, polarity: Positive, direction: Reverse })),
            (49, None), (50, None), (51, None), (52, None), (53, None),
            (54, Some(MelodicFigure { shape: ZigZag2, polarity: Positive, direction: Forward })),
            (57, None), (58, None),
            (59, Some(MelodicFigure { shape: Auxiliary, polarity: Positive, direction: Forward })),
            (61, None), (62, None), (63, None),
            (64, Some(MelodicFigure { shape: Run, polarity: Negative, direction: Forward })),
            (67, None), (68, None)].iter().copied().collect();
        test_emphasized(COUNTDOWN_MELODY, &expected);
    }

    #[test]
    fn test_emphasized_1() {
        let expected = vec![
            (0, Some(MelodicFigure { shape: LeapingScale, polarity: Positive, direction: Reverse })),
            (3, Some(MelodicFigure { shape: LeapingAux2, polarity: Negative, direction: Reverse })),
            (6, Some(MelodicFigure { shape: ReturnCrazyDriver, polarity: Negative, direction: Forward })),
            (9, None), (10, None),
            (11, Some(MelodicFigure { shape: LeapingScale, polarity: Positive, direction: Reverse })),
            (14, None), (15, None),
            (16, Some(MelodicFigure { shape: LeapingScale, polarity: Negative, direction: Forward })),
            (19, None),
            (20, Some(MelodicFigure { shape: LeapingScale, polarity: Positive, direction: Reverse })),
            (23, None),
            (24, Some(MelodicFigure { shape: ReturnCrazyDriver, polarity: Negative, direction: Reverse })),
            (27, Some(MelodicFigure { shape: Auxiliary, polarity: Positive, direction: Forward })),
            (29, None), (30, None), (31, None),
            (32, Some(MelodicFigure { shape: Run, polarity: Negative, direction: Forward })),
            (35, Some(MelodicFigure { shape: Trill1, polarity: Positive, direction: Forward })),
            (38, None), (39, None), (40, None), (41, None)
        ].iter().copied().collect();

        test_emphasized(EXAMPLE_MELODY, &expected);
    }


    fn test_emphasized(melody_str: &str, expected: &VecDeque<(usize,Option<MelodicFigure>)>) {
        let melody = Melody::from(melody_str).without_silence();
        let maker = MelodyMaker::new();
        let pauses = melody.find_pause_indices();
        println!("pauses: {pauses:?}");
        let chain = maker.emphasis_figure_chain(&melody, &pauses);
        assert!(MelodyMaker::is_chain(&chain));
        let notes = melody.notes.iter().enumerate().collect::<Vec<_>>();
        println!("{notes:?}");
        assert_eq!(&chain, expected);
    }

    fn test_variation_unchanged<V:Fn(&mut MelodyMaker,&Melody,f64)->Melody>(v_func: V) {
        let expected = "[[66, 0.42, 1],[73, 0.17, 1],[71, 0.13, 0.77],[73, 0.45, 0.41],[66, 0.85, 0.8],[74, 0.16, 1],[74, 0.37, 0.87],[73, 0.2, 1],[71, 0.03, 0.06],[71, 0.93, 1],[74, 0.16, 1],[73, 0.13, 1],[74, 0.45, 1],[66, 0.58, 0.8],[71, 0.15, 0.75],[71, 0.13, 0.81],[71, 0.21, 1],[69, 0.24, 0.94],[68, 0.22, 0.65],[71, 0.24, 1],[69, 0.68, 1],[73, 0.16, 1],[71, 0.14, 0.91],[73, 0.29, 1],[66, 0.61, 0.64],[74, 0.15, 0.87],[74, 0.14, 0.83],[74, 0.2, 1],[73, 0.29, 0.96],[72, 0.04, 0.49],[71, 1.01, 1],[74, 0.14, 0.94],[73, 0.13, 0.8],[74, 0.49, 1],[66, 0.93, 0.54],[71, 0.16, 0.81],[71, 0.13, 0.79],[71, 0.21, 0.87],[69, 0.24, 0.86],[68, 0.24, 0.67],[71, 0.24, 1],[69, 0.75, 0.86],[68, 0.18, 0.71],[69, 0.16, 0.89],[71, 0.02, 0.99],[83, 0.01, 1],[71, 0.56, 0.98],[69, 0.19, 1],[71, 0.2, 1],[73, 0.24, 1],[72, 0.03, 0.62],[71, 0.2, 0.91],[69, 0.01, 0.06],[69, 0.18, 0.73],[68, 0.19, 0.46],[66, 0.51, 0.76],[74, 0.56, 1],[73, 1.09, 0.79],[75, 0.16, 0.9],[73, 0.16, 0.84],[71, 0.18, 0.57],[73, 0.78, 0.64],[73, 0.14, 0.91],[73, 0.14, 0.87],[73, 0.26, 0.81],[71, 0.23, 0.91],[69, 0.19, 0.98],[68, 0.23, 0.59],[66, 1.22, 0.68]]";
        let melody = Melody::from(COUNTDOWN_MELODY).without_silence();
        let mut maker = MelodyMaker::new();
        let v = v_func(&mut maker, &melody, 0.0);
        assert_eq!(v.len(), melody.len());
        assert_eq!(v.sonic_pi_list().as_str(), expected);
    }

    fn test_variation_changed<V:Fn(&mut MelodyMaker,&Melody,f64)->Melody>(v_func: V, expected_lo: f64, expected_hi: f64) {
        let melody = Melody::from(COUNTDOWN_MELODY).without_silence();
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
                assert_eq!(var[i].intensity, melody[i].intensity);
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
    fn test_variation_1() {
        test_variation_unchanged(MelodyMaker::create_variation_1);
        test_variation_changed(MelodyMaker::create_variation_1, 0.25, 0.43);
    }

    #[test]
    fn test_variation_2() {
        test_variation_unchanged(MelodyMaker::create_variation_2);
        test_variation_changed(MelodyMaker::create_variation_2, 0.20, 0.35);
    }

    #[test]
    fn test_variation_4() {
        test_variation_unchanged(MelodyMaker::create_variation_4);
        test_variation_changed(MelodyMaker::create_variation_4, 0.08, 0.37);
    }

    #[test]
    fn study_figures() {
        let melody = Melody::from(EXAMPLE_MELODY).without_silence();
        let scale = melody.best_scale_for();
        let maker = MelodyMaker::new();
        let mut start = 0;
        let mut figures = Vec::new();
        while start < melody.len() {
            if let Some(matched) = maker.matching_figure(&melody, start) {
                figures.push((start, Some(matched),
                              matched.make_pitches(melody[start].pitch, &scale)));
                start += matched.len() - 1;
            } else {
                figures.push((start, None, vec![melody[start].pitch]));
                start += 1;
            }
        }
        for (i, (start, figure, notes)) in figures.iter().enumerate() {
            println!("Item {} Index {}", i, start);
            println!("{:?} {:?}", figure, notes);
            println!();
        }
    }

    #[test]
    fn study_subs() {
        let melody = Melody::from(EXAMPLE_MELODY).without_silence();
        let subs = melody.get_subdivisions();
        let scale = melody.best_scale_for();
        println!("Scale: {:?}", scale);
        for sub in subs {
            println!("{}", sub.view_notes());
            let end = sub.notes.last().unwrap().pitch;
            println!("Degree: {}", scale.diatonic_degree(end)
                .map(|d| format!("{}", d))
                .unwrap_or(String::from("chromatic")));
        }
    }
}
