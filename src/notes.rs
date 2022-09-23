use std::collections::{BTreeMap, HashMap, VecDeque};
use bare_metal_modulo::{MNum, ModNumC};
use ordered_float::OrderedFloat;
use histogram_macros::*;
use enum_iterator::{Sequence, all};

const NOTES_PER_OCTAVE: i16 = 12;
const USIZE_NOTES_PER_OCTAVE: usize = NOTES_PER_OCTAVE as usize;
const DIATONIC_SCALE_SIZE: usize = 7;
const DIATONIC_SCALE_HOPS: [i16; DIATONIC_SCALE_SIZE] = [2, 2, 1, 2, 2, 2, 1];
const NOTE_NAMES: [&str; NOTES_PER_OCTAVE as usize] = ["C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"];
const MODE_NAMES: [&str; DIATONIC_SCALE_SIZE] = ["ionian", "dorian", "phrygian", "lydian", "mixolydian", "aeolian", "locrian"];

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

    pub fn best_scale_for(&self) -> MusicMode {
        let mut note_weights = HashMap::new();
        for n in self.notes.iter() {
            bump_by!(note_weights, n.pitch % NOTES_PER_OCTAVE, n.duration);
        }
        let root = mode_by_weight!(note_weights).unwrap();
        let mut mode_weights = HashMap::new();
        for mode in MusicMode::all_modes_for(root).iter() {
            for n in self.notes.iter() {
                if mode.contains(n.pitch) {
                    bump_ref_by!(mode_weights, mode, n.duration);
                }
            }
        }
        mode_by_weight!(mode_weights).unwrap()
    }

    pub fn get_subdivisions(&self) -> Vec<Self> {
        let no_zeros = self.without_silence();
        let pauses = no_zeros.find_pause_indices();
        no_zeros.subdivide_using(&pauses)
    }

    fn find_pause_indices(&self) -> Vec<usize> {
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
    figure_tables: BTreeMap<usize, BTreeMap<i16, Vec<MelodicFigure>>>
}

impl MelodyMaker {
    pub fn new() -> Self {
        MelodyMaker {
            figure_tables: (3..=4).map(|len| (len, MelodicFigure::interval2figures(len))).collect()
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

    pub fn create_variation_1(&self, original: &Melody, p_rewrite: f64) -> Melody {
        assert!(0.0 <= p_rewrite && p_rewrite <= 1.0);
        let original = original.without_silence();
        let scale = original.best_scale_for();
        let mut figure_chain = self.greedy_figure_chain(&original);
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
                                self.pick_figure(figure.len(), figure.total_change())
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

    pub fn pick_figure(&self, figure_length: usize, jump: i16) -> MelodicFigure {
        let table = self.figure_tables.get(&figure_length).unwrap();
        let options = table.get(&jump).unwrap();
        options[rand::random::<usize>() % options.len()]
    }

    pub fn create_variation_2(&self, original: &Melody, p_rewrite: f64, p_3: f64) -> Melody {
        assert!(0.0 <= p_rewrite && p_rewrite <= 1.0);
        assert!(0.0 <= p_3 && p_3 <= 1.0);
        let original = original.without_silence();
        let scale = original.best_scale_for();
        let subs = original.get_subdivisions();
        original // temp return value
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
        assert!(pitch1 < 200);
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
        assert!(reference_pitch < 200);
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
    VaultDown4, VaultUp4, VaultDown5, VaultUp5, VaultDown6, VaultUp6,
    VaultDown4R, VaultUp4R, VaultDown5R, VaultUp5R, VaultDown6R, VaultUp6R,
    RollUpDown, RollDownUp, DoubleNeighbor1, DoubleNeighbor2,
    Double3rd1, Double3rd2,
    PendulumUpDown43, PendulumDownUp43, PendulumUpDown54, PendulumDownUp54,
    LeapingScale1, LeapingScale2, LeapingScale3, LeapingScale4,
    LeapingScale5, LeapingScale6, LeapingScale7, LeapingScale8,
    LeapingAuxLowerUp4, LeapingAuxUpperUp4, LeapingAuxUpLower4, LeapingAuxUpUpper4,
    LeapingAuxLowerDown4, LeapingAuxUpperDown4, LeapingAuxDownLower4, LeapingAuxDownUpper4,
    PendulumUpDownAuxDown4, PendulumUpDownAuxUp4, PendulumDownUpAuxDown4, PendulumDownUpAuxUp4,
    FunnelUp, FunnelDown,
    CambiataUp, CambiataDown,
    ZigZagUp, ZigZagDown
}

impl MelodicFigure {
    pub fn len(&self) -> usize {self.pattern().len() + 1}

    /// Returns the net change of diatonic steps from the start to the end of this `MelodicFigure`.
    pub fn total_change(&self) -> i16 {self.pattern().iter().sum()}

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
            MelodicFigure::VaultDown4             => vec![4, 1],
            MelodicFigure::VaultUp4               => vec![1, 4],
            MelodicFigure::VaultDown5             => vec![5, 1],
            MelodicFigure::VaultUp5               => vec![1, 5],
            MelodicFigure::VaultDown6             => vec![6, 1],
            MelodicFigure::VaultUp6               => vec![1, 6],
            MelodicFigure::VaultDown4R            => vec![-4, -1],
            MelodicFigure::VaultUp4R              => vec![-1, -4],
            MelodicFigure::VaultDown5R            => vec![-5, -1],
            MelodicFigure::VaultUp5R              => vec![-1, -5],
            MelodicFigure::VaultDown6R            => vec![-6, -1],
            MelodicFigure::VaultUp6R              => vec![-1, -6],
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
            MelodicFigure::CambiataUp             => vec![1, 2, -1],
            MelodicFigure::CambiataDown           => vec![-1, -2, 1],
            MelodicFigure::ZigZagUp               => vec![4, -2, 5],
            MelodicFigure::ZigZagDown             => vec![-4, 2, -5],
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use bare_metal_modulo::ModNumC;
    use crate::notes::{DIATONIC_SCALE_SIZE, MelodicFigure, Melody, MelodyMaker, MusicMode};

    const EXAMPLE_MELODY: &str = "55,0.39,0.91,55,0.04,0.0,59,0.33,0.73,60,0.06,0.44,62,0.02,0.87,59,0.05,0.0,60,0.16,0.0,62,0.2,0.0,55,0.39,0.61,55,0.01,0.0,57,0.34,0.98,57,0.05,0.0,55,0.39,0.78,54,0.02,0.98,55,0.19,0.0,54,0.12,0.0,52,0.11,0.74,52,0.0,0.0,54,0.12,0.46,54,0.03,0.0,50,0.1,0.84,50,0.27,0.0,55,0.27,0.74,55,0.1,0.0,59,0.27,0.44,60,0.07,0.54,62,0.04,0.91,59,0.09,0.0,60,0.11,0.0,62,0.19,0.0,55,0.29,0.67,55,0.07,0.0,57,0.32,0.76,57,0.06,0.0,55,0.23,0.7,55,0.05,0.0,54,0.12,0.93,54,0.07,0.0,50,0.37,0.8,50,0.5,0.0,55,0.36,0.76,55,0.05,0.0,59,0.28,0.76,60,0.05,0.7,62,0.01,0.91,59,0.07,0.0,60,0.15,0.0,62,0.2,0.0,55,0.33,0.67,55,0.02,0.0,57,0.29,0.8,57,0.1,0.0,55,0.29,0.9,55,0.08,0.0,54,0.16,1.0,54,0.12,0.0,52,0.12,0.72,54,0.01,0.71,52,0.14,0.0,54,0.07,0.0,50,0.1,0.76,50,0.23,0.0,55,0.22,0.65,55,0.13,0.0,57,0.29,0.64,57,0.08,0.0,55,0.23,0.76,55,0.07,0.0,54,0.12,0.99,54,0.04,0.0,52,0.24,0.95,52,0.19,0.0,54,0.13,1.0,54,0.15,0.0,52,0.12,0.72,52,0.03,0.0,54,0.19,0.83,54,0.13,0.0,50,0.06,0.69,50,0.15,0.0,55,0.01,0.73,57,0.07,0.66,57,0.55,0.0,55,1.5,0.0";

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
        let maker = MelodyMaker::new();
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
        for (i, pitch) in [67, 69, 71, 72, 74, 76, 78, 79, 81, 83, 84, 86, 88, 90, 91].iter().enumerate() {
            assert_eq!((i % DIATONIC_SCALE_SIZE) as i16 + 1, scale.diatonic_degree(*pitch).unwrap());
        }
    }

    #[test]
    fn test_figure_match() {
        let melody = Melody::from(EXAMPLE_MELODY).without_silence();
        let scale = melody.best_scale_for();
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
                println!("first: {:?} last: {:?} start: {}: {:?}: figure: {:?}",
                         scale.diatonic_degree(pitches[0]),
                         scale.diatonic_degree(*pitches.last().unwrap()),
                         i, matched, pitches);
            }
        }
        println!("# figures: {}", figure_count);
        println!("# distinct figures: {}", figure_set.len());
        println!("figures: {:?}", figure_set);
        assert_eq!(figure_count, 29);
        assert_eq!(figure_set.len(), 12);
        assert_eq!(format!("{:?}", figure_set), "{Note3ScaleUp, AuxiliaryDown, RunDown, Trill1Up, Trill1Down, NP3DownSG, PivotUpDown, ReturnDownUp, CrazyDriverUpDown, LeapingScale2, LeapingScale3, LeapingAuxDownUpper4}");
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