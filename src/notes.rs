use std::cmp::max;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use bare_metal_modulo::{MNum, ModNumC};
use ordered_float::OrderedFloat;
use histogram_macros::*;
use enum_iterator::{Sequence, all};
use rand::prelude::SliceRandom;

const MAX_USABLE_PITCH: i16 = 200;
const NOTES_PER_OCTAVE: i16 = 12;
const USIZE_NOTES_PER_OCTAVE: usize = NOTES_PER_OCTAVE as usize;
const DIATONIC_SCALE_SIZE: usize = 7;
const DIATONIC_SCALE_HOPS: [i16; DIATONIC_SCALE_SIZE] = [2, 2, 1, 2, 2, 2, 1];
const NOTE_NAMES: [&str; NOTES_PER_OCTAVE as usize] = ["C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"];
const MODE_NAMES: [&str; DIATONIC_SCALE_SIZE] = ["ionian", "dorian", "phrygian", "lydian", "mixolydian", "aeolian", "locrian"];

fn assert_prob(p: f64) {
    assert!(0.0 <= p && p <= 1.0);
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Note {
    pitch: i16,
    duration: OrderedFloat<f64>,
    intensity: OrderedFloat<f64>
}

impl Note {
    pub fn pitch(&self) -> i16 {self.pitch}

    pub fn repitched(&self, new_pitch: i16) -> Note {
        Note {pitch: new_pitch, duration: self.duration, intensity: self.intensity}
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
        let mut note_iter = self.notes.iter().map(|n| (n.pitch % NOTES_PER_OCTAVE, n.duration));
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

    pub fn find_pause_indices(&self) -> Vec<usize> {
        (1..(self.notes.len() - 1))
            .filter(|i| self[*i-1].duration < self[*i].duration &&
                self[*i].duration > self[*i+1].duration)
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

    pub fn randomized_figure_chain(&self, melody: &Melody, p_pick: f64) -> VecDeque<(usize,Option<MelodicFigure>)> {
        assert_prob(p_pick);
        let all_matched = self.all_figure_matches(melody);
        let mut v_i = 0;
        let mut result = VecDeque::new();
        let mut min_safe_i = 0;
        for i in 0..melody.len() {
            result.push_back((i, if v_i < all_matched.len() && i == all_matched[v_i].0 {
                let figure = all_matched[v_i].1;
                v_i += 1;
                if i >= min_safe_i && rand::random::<f64>() < p_pick {min_safe_i = i + figure.len() - 1; Some(figure)} else {None}
            } else {None}));
        }
        result
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
        (0..melody.len()).map(|i| (i, keepers.get(&i).copied())).collect()
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
        (0..melody.len()).map(|i| (i, keepers.get(&i).copied())).collect()
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

    pub fn create_variation_3(&mut self, original: &Melody, p_pick: f64) -> Melody {
        self.chain_variation_creator(original, 1.0, |s, m| s.randomized_figure_chain(m, p_pick), Self::pick_figure)
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
        assert!(pitch1 < MAX_USABLE_PITCH);
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
        assert!(reference_pitch < MAX_USABLE_PITCH);
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
// Some of these will need more attention, most especially the ones where there is a note that
// can jump just about anywhere:
// * Arch, LeapingScale, LeapingAux, PendulumAux, ZigZag
#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence, Hash, Ord, PartialOrd)]
pub enum MelodicFigure {
    Note3ScaleUp, Note3ScaleDown, AuxiliaryDown, AuxiliaryUp,
    ArpeggioUp, ArpeggioDown, RunUp, RunDown,
    Trill1Up, Trill2Up, Trill1Down, Trill2Down,
    Arch1, Arch2, Arch3, Arch4,
    NP3DownGS, NP3DownSG, NP3UpGS, NP3UpSG,
    PivotUpDown, PivotDownUp,
    LHPUpDown, LHPDownUp,
    ReturnUpDown, ReturnDownUp,
    CrazyDriverDownUp, CrazyDriverUpDown,
    ArpeggioPlusUpDown, ArpeggioPlusDownUp,
    ParkourBounce1, ParkourBounce2, ParkourPounce1, ParkourPounce2,
    ParkourBounce1R, ParkourBounce2R, ParkourPounce1R, ParkourPounce2R,
    VaultDown4, VaultUp4, VaultDown5, VaultUp5, VaultDown6, VaultUp6,
    VaultDown4R, VaultUp4R, VaultDown5R, VaultUp5R, VaultDown6R, VaultUp6R, VaultUp7, VaultUp7R,
    RollUpDown, RollDownUp, DoubleNeighbor1, DoubleNeighbor2,
    Double3rd1, Double3rd2,
    PendulumUpDown43, PendulumDownUp43, PendulumUpDown54, PendulumDownUp54,
    LeapingScale1, LeapingScale2, LeapingScale3, LeapingScale4,
    LeapingScale5, LeapingScale6, LeapingScale7, LeapingScale8,
    LeapingAuxLowerUp4, LeapingAuxUpperUp4, LeapingAuxUpLower4, LeapingAuxUpUpper4,
    LeapingAuxLowerDown4, LeapingAuxUpperDown4, LeapingAuxDownLower4, LeapingAuxDownUpper4,
    PendulumUpDownAuxDown4, PendulumUpDownAuxUp4, PendulumDownUpAuxDown4, PendulumDownUpAuxUp4,
    FunnelUp, FunnelDown,
    CambiataUpUp2, CambiataUpDown4, CambiataDownDown2, CambiataDownUp4,
    ZigZagUp, ZigZagDown, ZigZagDownLeapUp5, ZigZagUpLeapDown5,
    MysteryCountdown
}

impl MelodicFigure {
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
        melody.notes_left_from(start) >= self.len() &&
            self.pattern().iter().enumerate()
                .all(|(i, interval)| scale.diatonic_steps_between(melody[start + i].pitch, melody[start + i + 1].pitch)
                    .map_or(false, |actual| actual == *interval))
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

    pub fn pattern(&self) -> Vec<i16> {
        match self {
            MelodicFigure::Note3ScaleUp           => vec![1, 1],
            MelodicFigure::Note3ScaleDown         => vec![-1, -1],
            MelodicFigure::AuxiliaryDown          => vec![-1, 1],
            MelodicFigure::AuxiliaryUp            => vec![1, -1],
            MelodicFigure::ArpeggioUp             => vec![2, 2],
            MelodicFigure::ArpeggioDown           => vec![-2, -2],
            MelodicFigure::RunUp                  => vec![1, 1, 1],
            MelodicFigure::RunDown                => vec![-1, -1, -1],
            MelodicFigure::Trill1Up               => vec![1, -1, 1],
            MelodicFigure::Trill2Up               => vec![2, -2, 2],
            MelodicFigure::Trill1Down             => vec![-1, 1, -1],
            MelodicFigure::Trill2Down             => vec![-2, 2, -2],
            MelodicFigure::Arch1                  => vec![2, 2, -2],
            MelodicFigure::Arch2                  => vec![-2, -2, 2],
            MelodicFigure::Arch3                  => vec![2, -2, -2],
            MelodicFigure::Arch4                  => vec![-2, 2, 2],
            MelodicFigure::NP3DownGS              => vec![-2, -1],
            MelodicFigure::NP3DownSG              => vec![-1, -2],
            MelodicFigure::NP3UpGS                => vec![2, 1],
            MelodicFigure::NP3UpSG                => vec![1, 2],
            MelodicFigure::PivotUpDown            => vec![1, -2],
            MelodicFigure::PivotDownUp            => vec![-1, 2],
            MelodicFigure::LHPUpDown              => vec![2, -1],
            MelodicFigure::LHPDownUp              => vec![-2, 1],
            MelodicFigure::ReturnUpDown           => vec![1, 1, -1],
            MelodicFigure::ReturnDownUp           => vec![-1, -1, 1],
            MelodicFigure::CrazyDriverDownUp      => vec![-1, 1, 1],
            MelodicFigure::CrazyDriverUpDown      => vec![1, -1, -1],
            MelodicFigure::ArpeggioPlusUpDown     => vec![2, 2, -1],
            MelodicFigure::ArpeggioPlusDownUp     => vec![-2, -2, 1],
            MelodicFigure::ParkourPounce1         => vec![-1, 3],
            MelodicFigure::ParkourPounce2         => vec![1, -6],
            MelodicFigure::ParkourBounce1         => vec![3, -1],
            MelodicFigure::ParkourBounce2         => vec![-5, 1],
            MelodicFigure::ParkourPounce1R        => vec![1, -3],
            MelodicFigure::ParkourPounce2R        => vec![-1, 6],
            MelodicFigure::ParkourBounce1R        => vec![-3, 1],
            MelodicFigure::ParkourBounce2R        => vec![5, -1],
            MelodicFigure::VaultDown4             => vec![4, 1],
            MelodicFigure::VaultUp4               => vec![1, 4],
            MelodicFigure::VaultDown5             => vec![5, 1],
            MelodicFigure::VaultUp5               => vec![1, 5],
            MelodicFigure::VaultDown6             => vec![6, 1],
            MelodicFigure::VaultUp6               => vec![1, 6],
            MelodicFigure::VaultUp7               => vec![1, 7],
            MelodicFigure::VaultDown4R            => vec![-4, -1],
            MelodicFigure::VaultUp4R              => vec![-1, -4],
            MelodicFigure::VaultDown5R            => vec![-5, -1],
            MelodicFigure::VaultUp5R              => vec![-1, -5],
            MelodicFigure::VaultDown6R            => vec![-6, -1],
            MelodicFigure::VaultUp6R              => vec![-1, -6],
            MelodicFigure::VaultUp7R              => vec![-1, -7],
            MelodicFigure::RollUpDown             => vec![1, 1, -2],
            MelodicFigure::RollDownUp             => vec![-1, -1, 2],
            MelodicFigure::DoubleNeighbor1        => vec![1, -2, 1],
            MelodicFigure::DoubleNeighbor2        => vec![-1, 2, -1],
            MelodicFigure::Double3rd1             => vec![2, -1, 2],
            MelodicFigure::Double3rd2             => vec![-2, 1, -2],
            MelodicFigure::PendulumUpDown43       => vec![4, -3],
            MelodicFigure::PendulumDownUp43       => vec![-4, 3],
            MelodicFigure::PendulumUpDown54       => vec![5, -4],
            MelodicFigure::PendulumDownUp54       => vec![-5, 4],
            MelodicFigure::LeapingScale1          => vec![1, 1, 2],
            MelodicFigure::LeapingScale2          => vec![-1, -1, -2],
            MelodicFigure::LeapingScale3          => vec![2, 1, 1],
            MelodicFigure::LeapingScale4          => vec![-2, -1, -1],
            MelodicFigure::LeapingScale5          => vec![1, 1, -2],
            MelodicFigure::LeapingScale6          => vec![-1, -1, 2],
            MelodicFigure::LeapingScale7          => vec![-2, 1, 1],
            MelodicFigure::LeapingScale8          => vec![2, -1, -1],
            MelodicFigure::LeapingAuxLowerUp4     => vec![-1, 1, 4],
            MelodicFigure::LeapingAuxUpperUp4     => vec![1, -1, 4],
            MelodicFigure::LeapingAuxUpLower4     => vec![4, -1, 1],
            MelodicFigure::LeapingAuxUpUpper4     => vec![4, 1, -1],
            MelodicFigure::LeapingAuxLowerDown4   => vec![-1, 1, -4],
            MelodicFigure::LeapingAuxUpperDown4   => vec![1, -1, -4],
            MelodicFigure::LeapingAuxDownLower4   => vec![-4, -1, 1],
            MelodicFigure::LeapingAuxDownUpper4   => vec![-4, 1, -1],
            MelodicFigure::PendulumUpDownAuxDown4 => vec![4, -5, 1],
            MelodicFigure::PendulumUpDownAuxUp4   => vec![4, -3, -1],
            MelodicFigure::PendulumDownUpAuxDown4 => vec![-4, 3, 1],
            MelodicFigure::PendulumDownUpAuxUp4   => vec![-4, 5, -1],
            MelodicFigure::FunnelUp               => vec![3, -2, 1],
            MelodicFigure::FunnelDown             => vec![-3, 2, -1],
            MelodicFigure::CambiataUpUp2          => vec![1, 2, -1],
            MelodicFigure::CambiataDownDown2      => vec![-1, -2, 1],
            MelodicFigure::CambiataUpDown4        => vec![1, -4, 5],
            MelodicFigure::CambiataDownUp4        => vec![-1, 4, -5],
            MelodicFigure::ZigZagUp               => vec![4, -2, 5],
            MelodicFigure::ZigZagDown             => vec![-4, 2, -5],
            MelodicFigure::ZigZagDownLeapUp5      => vec![-1, 5, -1],
            MelodicFigure::ZigZagUpLeapDown5      => vec![1, -5, 1],
            MelodicFigure::MysteryCountdown       => vec![1, -5, 3]
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeSet, VecDeque};
    use bare_metal_modulo::ModNumC;
    use crate::notes::{DIATONIC_SCALE_SIZE, MelodicFigure, Melody, MelodyMaker, MusicMode};
    use crate::notes::MelodicFigure::{LeapingScale3, MysteryCountdown};

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
            (127, 1, 129), (129, 1, 131), (131, 1, 132), (120, 1, 122), (122, 1, 124)
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
        assert_eq!(format!("{:?}", figure_set), "{Note3ScaleUp, AuxiliaryDown, RunDown, Trill1Up, Trill1Down, NP3DownSG, PivotUpDown, ReturnDownUp, CrazyDriverUpDown, LeapingScale2, LeapingScale3, LeapingAuxDownUpper4}");
    }

    #[test]
    fn test_figure_match_countdown() {
        let (figure_count, figure_set) = test_figure_match(COUNTDOWN_MELODY);
        assert_eq!(figure_count, 33);
        assert_eq!(figure_set.len(), 18);
        assert_eq!(format!("{:?}", figure_set), "{Note3ScaleUp, Note3ScaleDown, AuxiliaryDown, RunDown, LHPUpDown, ReturnDownUp, CrazyDriverDownUp, ParkourBounce2R, VaultUp7, RollDownUp, DoubleNeighbor2, Double3rd1, LeapingScale8, LeapingAuxUpLower4, LeapingAuxLowerDown4, CambiataUpDown4, ZigZagDownLeapUp5, MysteryCountdown}");
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
            (1, Some(MelodicFigure::LeapingAuxLowerDown4)),
            (2, None),
            (3, None),
            (4, None),
            (5, None),
            (6, Some(MelodicFigure::Note3ScaleDown)),
            (7, None),
            (8, None),
            (9, Some(MelodicFigure::LHPUpDown)),
            (10, None),
            (11, Some(MelodicFigure::MysteryCountdown)),
            (12, None),
            (13, None),
            (14, None),
            (15, None),
            (16, None),
            (17, Some(MelodicFigure::DoubleNeighbor2)),
            (18, None),
            (19, None),
            (20, None),
            (21, Some(MelodicFigure::LeapingAuxLowerDown4)),
            (22, None),
            (23, None),
            (24, None),
            (25, None),
            (26, None),
            (27, None),
            (28, None),
            (29, None),
            (30, Some(MelodicFigure::LHPUpDown)),
            (31, None),
            (32, Some(MysteryCountdown)),
            (33, None),
            (34, None),
            (35, None),
            (36, None),
            (37, None),
            (38, Some(MelodicFigure::DoubleNeighbor2)),
            (39, None),
            (40, None),
            (41, Some(MelodicFigure::CrazyDriverDownUp)),
            (42, None),
            (43, None),
            (44, None),
            (45, None),
            (46, Some(MelodicFigure::CrazyDriverDownUp)),
            (47, None),
            (48, None),
            (49, None),
            (50, None),
            (51, None),
            (52, None),
            (53, None),
            (54, Some(MelodicFigure::ZigZagDownLeapUp5)),
            (55, None),
            (56, None),
            (57, None),
            (58, None),
            (59, Some(MelodicFigure::AuxiliaryDown)),
            (60, None),
            (61, None),
            (62, None),
            (63, None),
            (64, Some(MelodicFigure::RunDown)),
            (65, None),
            (66, None),
            (67, None),
            (68, None)].iter().copied().collect();
        test_emphasized(COUNTDOWN_MELODY, &expected);
    }

    #[test]
    fn test_emphasized_1() {
        let expected = vec![
            (0, Some(MelodicFigure::LeapingScale3)),
            (1, None), (2, None),
            (3, Some(MelodicFigure::LeapingAuxDownUpper4)),
            (4, None), (5, None), (6, Some(MelodicFigure::ReturnDownUp)), (7, None), (8, None),
            (9, None), (10, None), (11, Some(MelodicFigure::LeapingScale3)), (12, None), (13, None),
            (14, None), (15, None), (16, Some(MelodicFigure::LeapingScale2)), (17, None), (18, None),
            (19, None), (20, Some(LeapingScale3)), (21, None), (22, None), (23, None),
            (24, Some(MelodicFigure::CrazyDriverUpDown)), (25, None), (26, None),
            (27, Some(MelodicFigure::AuxiliaryDown)), (28, None), (29, None), (30, None), (31, None),
            (32, Some(MelodicFigure::RunDown)),
            (33, None), (34, None),
            (35, Some(MelodicFigure::Trill1Up)), (36, None), (37, None), (38, None), (39, None),
            (40, None), (41, None)
        ].iter().copied().collect();
        test_emphasized(EXAMPLE_MELODY, &expected);
    }

    #[test]
    fn test_interfere() {
        assert!(!MelodicFigure::DoubleNeighbor2.interfere(17, MelodicFigure::LHPUpDown, 20));
        assert!(MelodicFigure::CrazyDriverDownUp.interfere(46, MelodicFigure::Note3ScaleUp, 47));
    }

    fn test_emphasized(melody_str: &str, expected: &VecDeque<(usize,Option<MelodicFigure>)>) {
        let melody = Melody::from(melody_str).without_silence();
        let maker = MelodyMaker::new();
        let pauses = melody.find_pause_indices();
        let chain = maker.emphasis_figure_chain(&melody, &pauses);
        assert_eq!(&chain, expected);
    }

    #[test]
    fn test_variation_4_no_change() {
        let expected = "[[66, 0.42, 1],[73, 0.17, 1],[71, 0.13, 0.77],[73, 0.45, 0.41],[66, 0.85, 0.8],[74, 0.16, 1],[74, 0.37, 0.87],[73, 0.2, 1],[71, 0.03, 0.06],[71, 0.93, 1],[74, 0.16, 1],[73, 0.13, 1],[74, 0.45, 1],[66, 0.58, 0.8],[71, 0.15, 0.75],[71, 0.13, 0.81],[71, 0.21, 1],[69, 0.24, 0.94],[68, 0.22, 0.65],[71, 0.24, 1],[69, 0.68, 1],[73, 0.16, 1],[71, 0.14, 0.91],[73, 0.29, 1],[66, 0.61, 0.64],[74, 0.15, 0.87],[74, 0.14, 0.83],[74, 0.2, 1],[73, 0.29, 0.96],[72, 0.04, 0.49],[71, 1.01, 1],[74, 0.14, 0.94],[73, 0.13, 0.8],[74, 0.49, 1],[66, 0.93, 0.54],[71, 0.16, 0.81],[71, 0.13, 0.79],[71, 0.21, 0.87],[69, 0.24, 0.86],[68, 0.24, 0.67],[71, 0.24, 1],[69, 0.75, 0.86],[68, 0.18, 0.71],[69, 0.16, 0.89],[71, 0.02, 0.99],[83, 0.01, 1],[71, 0.56, 0.98],[69, 0.19, 1],[71, 0.2, 1],[73, 0.24, 1],[72, 0.03, 0.62],[71, 0.2, 0.91],[69, 0.01, 0.06],[69, 0.18, 0.73],[68, 0.19, 0.46],[66, 0.51, 0.76],[74, 0.56, 1],[73, 1.09, 0.79],[75, 0.16, 0.9],[73, 0.16, 0.84],[71, 0.18, 0.57],[73, 0.78, 0.64],[73, 0.14, 0.91],[73, 0.14, 0.87],[73, 0.26, 0.81],[71, 0.23, 0.91],[69, 0.19, 0.98],[68, 0.23, 0.59],[66, 1.22, 0.68]]";
        let melody = Melody::from(COUNTDOWN_MELODY).without_silence();
        let mut maker = MelodyMaker::new();
        let v = maker.create_variation_4(&melody, 0.0);
        assert_eq!(v.sonic_pi_list().as_str(), expected);
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