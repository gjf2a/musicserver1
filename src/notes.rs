use std::collections::{BTreeMap, HashMap};
use bare_metal_modulo::{MNum, ModNumC};
use ordered_float::OrderedFloat;
use histogram_macros::*;
use enum_iterator::{Sequence, all};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Note {
    note: i16,
    duration: OrderedFloat<f64>,
    intensity: OrderedFloat<f64>
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
                    notes.push(Note {note, duration, intensity});
                }
                _ => {break;}
            }
        }
        Melody {notes}
    }

    pub fn len(&self) -> usize {self.notes.len()}

    pub fn view_notes(&self) -> String {
        let mut result = String::new();
        for n in self.notes.iter() {
            result.push_str(NOTE_NAMES[(n.note % NOTES_PER_OCTAVE) as usize]);
            result.push_str(" ");
        }
        result
    }

    pub fn get_subdivisions(&self) -> Vec<Self> {
        let no_zeros = self.without_silence();
        let pauses = no_zeros.find_pause_indices();
        no_zeros.subdivide_using(&pauses)
    }

    pub fn without_silence(&self) -> Self {
        Melody {notes: self.notes.iter().filter(|n| n.intensity > OrderedFloat(0.0)).copied().collect()}
    }

    pub fn create_variation(&self, p_rewrite: f64) -> Self {
        assert!(0.0 <= p_rewrite && p_rewrite <= 1.0);
        let subs = self.get_subdivisions();
        let mut result = Melody {notes: Vec::new()};
        for sub in subs.iter() {
            if rand::random::<f64>() < p_rewrite {

            } else {
                for note in sub.notes.iter().copied() {
                    result.notes.push(note);
                }
            }
        }
        result
    }

    fn find_pause_indices(&self) -> Vec<usize> {
        (1..(self.notes.len() - 1))
            .filter(|i| self.notes[*i-1].duration < self.notes[*i].duration &&
                self.notes[*i].duration > self.notes[*i+1].duration)
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

    pub fn best_scale_for(&self) -> MusicMode {
        let mut note_weights = HashMap::new();
        for n in self.notes.iter() {
            bump_by!(note_weights, n.note % NOTES_PER_OCTAVE, n.duration);
        }
        let root = mode_by_weight!(note_weights).unwrap();
        let mut mode_weights = HashMap::new();
        for mode in MusicMode::all_modes_for(root).iter() {
            for n in self.notes.iter() {
                if mode.contains(n.note) {
                    bump_ref_by!(mode_weights, mode, n.duration);
                }
            }
        }
        mode_by_weight!(mode_weights).unwrap()
    }
}

const NOTES_PER_OCTAVE: i16 = 12;
const DIATONIC_SCALE_SIZE: usize = 7;
const DIATONIC_SCALE_HOPS: [i16; DIATONIC_SCALE_SIZE] = [2, 2, 1, 2, 2, 2, 1];
const NOTE_NAMES: [&str; NOTES_PER_OCTAVE as usize] = ["C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"];
const MODE_NAMES: [&str; DIATONIC_SCALE_SIZE] = ["ionian", "dorian", "phrygian", "lydian", "mixolydian", "aeolian", "locrian"];

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct MusicMode {
    root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>,
    octave_notes: [i16; DIATONIC_SCALE_SIZE]
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
        let mut octave_notes = [root_note; DIATONIC_SCALE_SIZE];
        let mut offset = DIATONIC_SCALE_HOPS[root_pos.a()];
        for i in 1..octave_notes.len() {
            octave_notes[i] += offset;
            offset += DIATONIC_SCALE_HOPS[(root_pos + i).a()];
        }
        MusicMode {root_pos, octave_notes}
    }

    pub fn next_note(&self, reference_note: i16, scale_steps_away: i16) -> i16 {
        let mut octaves_up = reference_note / NOTES_PER_OCTAVE;
        match self.octave_notes.iter()
            .position(|p| *p == self.octave_note(reference_note)) {
            Some(i) => {
                let mut j = i as i16 + scale_steps_away;
                while j < 0 {
                    j += DIATONIC_SCALE_SIZE as i16;
                    octaves_up -= 1;
                }
                while j >= DIATONIC_SCALE_SIZE as i16 {
                    j -= DIATONIC_SCALE_SIZE as i16;
                    octaves_up += 1;
                }
                self.octave_notes[j as usize] + octaves_up * NOTES_PER_OCTAVE
            }
            None => {self.next_note(reference_note + 1, scale_steps_away)}
        }
    }

    pub fn note(&self, degree: u8) -> i16 {
        assert!(degree >= 1);
        let mut result = self.root();
        let mut to_next = self.root_pos;
        for _ in 0..(degree - 1) {
            result += DIATONIC_SCALE_HOPS[to_next.a()];
            to_next += 1;
        }
        result
    }

    fn root(&self) -> i16 {
        self.octave_notes[0]
    }

    fn octave_note(&self, note: i16) -> i16 {
        self.root() + note % NOTES_PER_OCTAVE
    }

    pub fn contains(&self, note: i16) -> bool {
        self.octave_notes.contains(&(self.octave_note(note)))
    }
}

// Inspired by: https://figuringoutmelody.com/the-24-universal-melodic-figures/
// Some of these will need more attention, most especially the ones where there is a note that
// can jump just about anywhere:
// * Arch, LeapingScale, LeapingAux, PendulumAux, ZigZag
#[derive(Copy, Clone, Eq, PartialEq, Debug, Sequence)]
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

    pub fn total_change(&self) -> i16 {self.pattern().iter().sum()}

    pub fn interval2figures() -> BTreeMap<i16, Vec<Self>> {
        let mut result = BTreeMap::new();
        for m in all::<MelodicFigure>() {
            let interval = m.total_change();
            match result.get_mut(&interval) {
                None => {result.insert(interval, vec![m]);}
                Some(v) => {v.push(m);}
            }
        }
        result
    }

    pub fn pattern(&self) -> Vec<i16> {
        match self {
            MelodicFigure::Note3ScaleUp   => vec![1, 1],
            MelodicFigure::Note3ScaleDown => vec![-1, -1],
            MelodicFigure::AuxiliaryDown  => vec![-1, 1],
            MelodicFigure::AuxiliaryUp    => vec![1, -1],
            MelodicFigure::ArpeggioUp     => vec![2, 2],
            MelodicFigure::ArpeggioDown   => vec![-2, -2],
            MelodicFigure::RunUp          => vec![1, 1, 1],
            MelodicFigure::RunDown        => vec![-1, -1, -1],
            MelodicFigure::Trill1Up       => vec![1, -1, 1],
            MelodicFigure::Trill2Up       => vec![2, -2, 2],
            MelodicFigure::Trill1Down     => vec![-1, 1, -1],
            MelodicFigure::Trill2Down     => vec![-2, 2, -2],
            MelodicFigure::Arch1          => vec![2, 2, -2],
            MelodicFigure::Arch2          => vec![-2, -2, 2],
            MelodicFigure::Arch3          => vec![2, -2, -2],
            MelodicFigure::Arch4          => vec![-2, 2, 2],
            MelodicFigure::NP3DownGS      => vec![-2, -1],
            MelodicFigure::NP3DownSG      => vec![-1, -2],
            MelodicFigure::NP3UpGS        => vec![2, 1],
            MelodicFigure::NP3UpSG        => vec![1, 2],
            MelodicFigure::PivotUpDown    => vec![1, -2],
            MelodicFigure::PivotDownUp    => vec![-1, 2],
            MelodicFigure::LHPUpDown      => vec![2, -1],
            MelodicFigure::LHPDownUp      => vec![-2, 1],
            MelodicFigure::ReturnUpDown   => vec![1, 1, -1],
            MelodicFigure::ReturnDownUp   => vec![-1, -1, 1],
            MelodicFigure::CrazyDriverDownUp => vec![-1, 1, 1],
            MelodicFigure::CrazyDriverUpDown => vec![1, -1, -1],
            MelodicFigure::ArpeggioPlusUpDown   => vec![2, 2, -1],
            MelodicFigure::ArpeggioPlusDownUp   => vec![-2, -2, 1],
            MelodicFigure::ParkourPounce1 => vec![-1, 3],
            MelodicFigure::ParkourPounce2 => vec![1, -6],
            MelodicFigure::ParkourBounce1 => vec![3, -1],
            MelodicFigure::ParkourBounce2 => vec![-5, 1],
            MelodicFigure::VaultDown4 => vec![4, 1],
            MelodicFigure::VaultUp4 => vec![1, 4],
            MelodicFigure::VaultDown5 => vec![5, 1],
            MelodicFigure::VaultUp5 => vec![1, 5],
            MelodicFigure::VaultDown6 => vec![6, 1],
            MelodicFigure::VaultUp6 => vec![1, 6],
            MelodicFigure::VaultDown4R => vec![-4, -1],
            MelodicFigure::VaultUp4R => vec![-1, -4],
            MelodicFigure::VaultDown5R => vec![-5, -1],
            MelodicFigure::VaultUp5R => vec![-1, -5],
            MelodicFigure::VaultDown6R => vec![-6, -1],
            MelodicFigure::VaultUp6R => vec![-1, -6],
            MelodicFigure::RollUpDown => vec![1, 1, -2],
            MelodicFigure::RollDownUp => vec![-1, -1, 2],
            MelodicFigure::DoubleNeighbor1 => vec![1, -2, 1],
            MelodicFigure::DoubleNeighbor2 => vec![-1, 2, -1],
            MelodicFigure::Double3rd1 => vec![2, -1, 2],
            MelodicFigure::Double3rd2 => vec![-2, 1, -2],
            MelodicFigure::PendulumUpDown43 => vec![4, -3],
            MelodicFigure::PendulumDownUp43 => vec![-4, 3],
            MelodicFigure::PendulumUpDown54 => vec![5, -4],
            MelodicFigure::PendulumDownUp54 => vec![-5, 4],
            MelodicFigure::LeapingScale1 => vec![1, 1, 2],
            MelodicFigure::LeapingScale2 => vec![-1, -1, -2],
            MelodicFigure::LeapingScale3 => vec![2, 1, 1],
            MelodicFigure::LeapingScale4 => vec![-2, -1, -1],
            MelodicFigure::LeapingScale5 => vec![1, 1, -2],
            MelodicFigure::LeapingScale6 => vec![-1, -1, 2],
            MelodicFigure::LeapingScale7 => vec![-2, 1, 1],
            MelodicFigure::LeapingScale8 => vec![2, -1, -1],
            MelodicFigure::LeapingAuxLowerUp4 => vec![-1, 1, 4],
            MelodicFigure::LeapingAuxUpperUp4 => vec![1, -1, 4],
            MelodicFigure::LeapingAuxUpLower4 => vec![4, -1, 1],
            MelodicFigure::LeapingAuxUpUpper4 => vec![4, 1, -1],
            MelodicFigure::LeapingAuxLowerDown4 => vec![-1, 1, -4],
            MelodicFigure::LeapingAuxUpperDown4 => vec![1, -1, -4],
            MelodicFigure::LeapingAuxDownLower4 => vec![-4, -1, 1],
            MelodicFigure::LeapingAuxDownUpper4 => vec![-4, 1, -1],
            MelodicFigure::PendulumUpDownAuxDown4 => vec![4, -5, 1],
            MelodicFigure::PendulumUpDownAuxUp4 => vec![4, -3, -1],
            MelodicFigure::PendulumDownUpAuxDown4 => vec![-4, 3, 1],
            MelodicFigure::PendulumDownUpAuxUp4 => vec![-4, 5, -1],
            MelodicFigure::FunnelUp     => vec![3, -2, 1],
            MelodicFigure::FunnelDown => vec![-3, 2, -1],
            MelodicFigure::CambiataUp => vec![1, 2, -1],
            MelodicFigure::CambiataDown => vec![-1, -2, 1],
            MelodicFigure::ZigZagUp => vec![4, -2, 5],
            MelodicFigure::ZigZagDown => vec![-4, 2, -5],
        }
    }
}

#[cfg(test)]
mod tests {
    use bare_metal_modulo::ModNumC;
    use crate::notes::{DIATONIC_SCALE_SIZE, MelodicFigure, Melody, MusicMode};

    #[test]
    fn test_parse_melody() {
        let m = "69,0.24,1.0,69,0.09,0.0,72,0.31,1.0,72,0.08,0.0,71,0.29,0.69";
        let notes = Melody::from(m);
        assert_eq!(format!("{:?}", notes), "Melody { notes: [Note { note: 69, duration: OrderedFloat(0.24), intensity: OrderedFloat(1.0) }, Note { note: 69, duration: OrderedFloat(0.09), intensity: OrderedFloat(0.0) }, Note { note: 72, duration: OrderedFloat(0.31), intensity: OrderedFloat(1.0) }, Note { note: 72, duration: OrderedFloat(0.08), intensity: OrderedFloat(0.0) }, Note { note: 71, duration: OrderedFloat(0.29), intensity: OrderedFloat(0.69) }] }");
    }

    #[test]
    fn test_subdivide_melody() {
        let example = "55,0.39,0.91,55,0.04,0.0,59,0.33,0.73,60,0.06,0.44,62,0.02,0.87,59,0.05,0.0,60,0.16,0.0,62,0.2,0.0,55,0.39,0.61,55,0.01,0.0,57,0.34,0.98,57,0.05,0.0,55,0.39,0.78,54,0.02,0.98,55,0.19,0.0,54,0.12,0.0,52,0.11,0.74,52,0.0,0.0,54,0.12,0.46,54,0.03,0.0,50,0.1,0.84,50,0.27,0.0,55,0.27,0.74,55,0.1,0.0,59,0.27,0.44,60,0.07,0.54,62,0.04,0.91,59,0.09,0.0,60,0.11,0.0,62,0.19,0.0,55,0.29,0.67,55,0.07,0.0,57,0.32,0.76,57,0.06,0.0,55,0.23,0.7,55,0.05,0.0,54,0.12,0.93,54,0.07,0.0,50,0.37,0.8,50,0.5,0.0,55,0.36,0.76,55,0.05,0.0,59,0.28,0.76,60,0.05,0.7,62,0.01,0.91,59,0.07,0.0,60,0.15,0.0,62,0.2,0.0,55,0.33,0.67,55,0.02,0.0,57,0.29,0.8,57,0.1,0.0,55,0.29,0.9,55,0.08,0.0,54,0.16,1.0,54,0.12,0.0,52,0.12,0.72,54,0.01,0.71,52,0.14,0.0,54,0.07,0.0,50,0.1,0.76,50,0.23,0.0,55,0.22,0.65,55,0.13,0.0,57,0.29,0.64,57,0.08,0.0,55,0.23,0.76,55,0.07,0.0,54,0.12,0.99,54,0.04,0.0,52,0.24,0.95,52,0.19,0.0,54,0.13,1.0,54,0.15,0.0,52,0.12,0.72,52,0.03,0.0,54,0.19,0.83,54,0.13,0.0,50,0.06,0.69,50,0.15,0.0,55,0.01,0.73,57,0.07,0.66,57,0.55,0.0,55,1.5,0.0";
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

        let notes = Melody::from(example);
        println!("{:?}", notes);
        let subs = notes.get_subdivisions();
        assert_eq!(subs.len(), expected_subs.len());
        for i in 0..expected_subs.len() {
            println!("sub length: {} {}", subs[i].notes.len(), subs[i].view_notes());
            assert_eq!(Melody::from(expected_subs[i]).without_silence(), subs[i]);
        }

        let best = notes.best_scale_for();
        println!("{} {:?}", best.name(), best);
    }

    #[test]
    fn test_scales() {
        let c5_major = MusicMode::new(ModNumC::new(0), 72);
        let c_notes: [i16; 8] = [72, 74, 76, 77, 79, 81, 83, 84];
        for (i, n) in c_notes.iter().enumerate() {
            let degree = (i + 1) as u8;
            assert_eq!(c5_major.note(degree), *n);
            assert!(c5_major.contains(*n));
        }

        let not_c_notes: [i16; 5] = [73, 75, 78, 80, 82];
        for n in not_c_notes.iter() {
            assert!(!c5_major.contains(*n));
        }
    }

    #[test]
    fn test_modes() {
        let modes = MusicMode::all_modes_for(72);
        let expected: [[i16; DIATONIC_SCALE_SIZE]; DIATONIC_SCALE_SIZE] = [
            [72, 74, 76, 77, 79, 81, 83],
            [72, 74, 75, 77, 79, 81, 82],
            [72, 73, 75, 77, 79, 80, 82],
            [72, 74, 76, 78, 79, 81, 83],
            [72, 74, 76, 77, 79, 81, 82],
            [72, 74, 75, 77, 79, 80, 82],
            [72, 73, 75, 77, 78, 80, 82]
        ];
        for i in 0..DIATONIC_SCALE_SIZE {
            assert_eq!(modes[i].octave_notes, expected[i]);
        }
    }

    #[test]
    fn test_interval_table() {
        let table = MelodicFigure::interval2figures();
        for (i, ms) in table.iter() {
            println!("{}: {:?}", *i, ms);
        }
    }
}