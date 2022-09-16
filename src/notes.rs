use bare_metal_modulo::{MNum, ModNumC};
use ordered_float::OrderedFloat;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Note {
    note: u8,
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

    pub fn get_subdivisions(&self) -> Vec<Self> {
        let no_zeros = self.without_silence();
        let pauses = no_zeros.find_pause_indices();
        no_zeros.subdivide_using(&pauses)
    }

    pub fn without_silence(&self) -> Self {
        Melody {notes: self.notes.iter().filter(|n| n.intensity > OrderedFloat(0.0)).copied().collect()}
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
}

const NOTES_PER_OCTAVE: u8 = 12;
const DIATONIC_SCALE_SIZE: usize = 7;
const DIATONIC_SCALE_HOPS: [u8; DIATONIC_SCALE_SIZE] = [2, 2, 1, 2, 2, 2, 1];

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Scale {
    root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>,
    root_note: u8,
    octave_notes: [u8; DIATONIC_SCALE_SIZE]
}

impl Scale {
    pub fn all_modes_for(root_note: u8) -> Vec<Self> {
        (0..DIATONIC_SCALE_SIZE)
            .map(|i| Self::new(ModNumC::new(i), root_note))
            .collect()
    }

    pub fn new(root_pos: ModNumC<usize, DIATONIC_SCALE_SIZE>, root_note: u8) -> Self {
        let mut octave_notes = [root_note; DIATONIC_SCALE_SIZE];
        let mut offset = DIATONIC_SCALE_HOPS[root_pos.a()];
        for i in 1..octave_notes.len() {
            octave_notes[i] += offset;
            offset += DIATONIC_SCALE_HOPS[(root_pos + i).a()];
        }
        Scale {root_note, root_pos, octave_notes}
    }

    pub fn note(&self, degree: u8) -> u8 {
        assert!(degree >= 1);
        let mut result = self.root_note;
        let mut to_next = self.root_pos;
        for _ in 0..(degree - 1) {
            result += DIATONIC_SCALE_HOPS[to_next.a()];
            to_next += 1;
        }
        result
    }

    pub fn contains(&self, note: u8) -> bool {
        self.octave_notes.contains(&(self.root_note + note % NOTES_PER_OCTAVE))
    }
}

#[cfg(test)]
mod tests {
    use bare_metal_modulo::ModNumC;
    use crate::notes::{DIATONIC_SCALE_SIZE, Melody, Scale};

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
        let subs = notes.get_subdivisions();
        assert_eq!(subs.len(), expected_subs.len());
        for i in 0..expected_subs.len() {
            assert_eq!(Melody::from(expected_subs[i]).without_silence(), subs[i]);
        }
    }

    #[test]
    fn test_scales() {
        let c5_major = Scale::new(ModNumC::new(0), 72);
        let c_notes: [u8; 8] = [72, 74, 76, 77, 79, 81, 83, 84];
        for (i, n) in c_notes.iter().enumerate() {
            let degree = (i + 1) as u8;
            assert_eq!(c5_major.note(degree), *n);
            assert!(c5_major.contains(*n));
        }

        let not_c_notes: [u8; 5] = [73, 75, 78, 80, 82];
        for n in not_c_notes.iter() {
            assert!(!c5_major.contains(*n));
        }
    }

    #[test]
    fn test_modes() {
        let modes = Scale::all_modes_for(72);
        let expected: [[u8; DIATONIC_SCALE_SIZE]; DIATONIC_SCALE_SIZE] = [
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
}