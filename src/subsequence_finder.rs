use std::fmt::Debug;
use trait_set::trait_set;
use vecmap::VecMap;

trait_set! {
    pub trait SeqItem = Eq + PartialEq + Debug + Copy + Clone;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Subsequences {
    length: usize,
    starts: Vec<usize>,
}

impl Subsequences {
    fn overlaps(&self, start: usize, length: usize) -> bool {
        let end = start + length - 1;
        self.starts.iter().any(|self_start| {
            let self_range = *self_start..(self_start + self.length);
            self_range.contains(&start) || self_range.contains(&end)
        })
    }

    fn remove_internal_overlaps(&mut self) {
        if self.starts.len() > 0 {
            let mut purged_starts = vec![self.starts[0]];
            for start in self.starts.iter().skip(1) {
                if !purged_starts.iter().any(|ps| ps + self.length > *start) {
                    purged_starts.push(*start);
                }
            }
            self.starts = purged_starts;
        }
    }

    fn remove_overlaps(&mut self, other: &Subsequences) {
        self.starts = self
            .starts
            .iter()
            .filter(|start| !other.overlaps(**start, self.length))
            .copied()
            .collect::<Vec<_>>();
    }

    fn num_repeats(&self) -> usize {
        self.starts.len()
    }

    pub fn sub_len(&self) -> usize {self.length}

    pub fn starts(&self) -> &Vec<usize> {&self.starts}
}

fn find_subs_of_length<T: SeqItem>(items: &Vec<T>, length: usize) -> Vec<Subsequences> {
    let mut value2seq = VecMap::new();
    for i in 0..=items.len() - length {
        let sub: Vec<T> = items[i..i + length].iter().copied().collect();
        match value2seq.get_mut(&sub) {
            None => {value2seq.insert(sub, Subsequences {length, starts: vec![i]});}
            Some(subseq) => {subseq.starts.push(i);}
        }
    }
    value2seq
        .drain(..)
        .map(|(_, seq)| seq)
        .filter(|seq| seq.starts.len() > 1)
        .collect()
}

pub fn find_maximal_repeated_subs<T: SeqItem>(items: &Vec<T>, min_repeats: usize, min_length: usize) -> Vec<Subsequences> {
    let mut result = vec![];
    for length in (min_length..=items.len()).rev() {
        for mut sub in find_subs_of_length(&items, length) {
            sub.remove_internal_overlaps();
            for result_sub in result.iter() {
                sub.remove_overlaps(result_sub);
            }
            if sub.num_repeats() >= min_repeats {
                result.push(sub);
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use std::ops::Range;
    use crate::subsequence_finder::{find_maximal_repeated_subs, Subsequences};
    use vecmap::VecSet;

    impl Subsequences {
        fn ranges(&self) -> Vec<Range<usize>> {
            self.starts.iter().map(|s| *s..*s + self.length).collect()
        }
    }

    fn all_independent(subs: &Vec<Subsequences>) -> bool {
        let mut ranges = vec![];
        for sub in subs.iter() {
            ranges.append(&mut sub.ranges());
        }
        (0..ranges.len()).all(|i| (0..i).all(|j| !ranges[i].contains(&ranges[j].start)))
    }

    #[test]
    fn test_finder() {
        let example = vec![
            0, 7, 0, -7, 0, 2, 0, -1, 0, -1, 0, -1, 0, -2, 0, 1, 0, -2, 0, 1, 0, 1, 0, 0, 0, 0, -1,
            0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0, -1, 0, -1, 0, 7, 0, -7, 0, -1, 0, -2, 0, 1, 0, -2,
            0, 1, 0, 1, 0, 0, 0, -1, 0, -1, 0, 1, 0, 1, 0, -2, 0,
        ];
        let subs = find_maximal_repeated_subs(&example, 2, 1);
        let expected: VecSet<Subsequences> = [
            (15, vec![10, 46]),
            (6, vec![4, 36]),
            (5, vec![25, 62]),
            (4, vec![0, 42]),
            (2, vec![31, 34]),
            (1, vec![30, 68, 70]),
        ]
        .iter()
        .map(|(length, starts)| Subsequences {
            length: *length,
            starts: starts.clone(),
        })
        .collect();

        assert_eq!(subs.len(), expected.len());

        for sub in subs.iter() {
            assert!(expected.contains(&sub));
        }
        assert!(all_independent(&subs));
    }

    #[test]
    fn lean_on_test() {
        let lean_on_intervals = vec![1, 1, 1, -1, -1, 1, -1, -1, 1, 1, -1, 1, -2, 1, 1, 1, -1, 1, -1, -1, 1, -4, 5, -1, -5, 4, -1];
        let subs = find_maximal_repeated_subs(&lean_on_intervals, 2, 2);
        assert_eq!(format!("{subs:?}"), "[Subsequences { length: 5, starts: [4, 16] }, Subsequences { length: 3, starts: [0, 13] }]");
        assert!(all_independent(&subs));
    }
}
