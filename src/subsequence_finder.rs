use std::fmt::Debug;
use std::hash::Hash;
use trait_set::trait_set;
use vecmap::VecMap;

trait_set! {
    pub trait SeqItem = Eq + PartialEq + Hash + Debug + Copy + Clone;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Subsequences {
    length: usize,
    starts: Vec<usize>
}

impl Subsequences {
    fn supersedes(&self, start: usize, length: usize) -> bool {
        self.starts.iter().any(|self_start| {
            let my_range = *self_start..(self_start + self.length);
            self.length >= length && my_range.contains(&start) && my_range.contains(&(start + length - 1))
        })
    }

    fn remove_superseded(&mut self, other: &Subsequences) {
        self.starts = self.starts.iter().filter(|start| !other.supersedes(**start, self.length)).copied().collect::<Vec<_>>();
    }

    fn len(&self) -> usize {self.starts.len()}
}

fn find_subs_of_length<T:SeqItem>(items: &Vec<T>, length: usize) -> Vec<Subsequences> {
    let mut value2seq = VecMap::new();
    for i in 0..=items.len() - length {
        let sub: Vec<T> = items[i..i+length].iter().copied().collect();
        match value2seq.get_mut(&sub) {
            None => {value2seq.insert(sub, Subsequences {length, starts: vec![i]});}
            Some(subseq) => {subseq.starts.push(i);}
        }
    }
    value2seq.drain(..).map(|(_,seq)| seq).filter(|seq| seq.starts.len() > 1).collect()
}

fn find_maximal_repeated_subs<T:SeqItem>(items: &Vec<T>) -> Vec<Subsequences> {
    let mut result = vec![];
    for length in (1..=items.len()).rev() {
        for mut sub in find_subs_of_length(&items, length) {
            for result_sub in result.iter() {
                sub.remove_superseded(result_sub);
            }
            if sub.len() > 1 {
                result.push(sub);
            }
        }
    }
    result
}


#[cfg(test)]
mod tests {
    use crate::subsequence_finder::{find_maximal_repeated_subs, Subsequences};
    use vecmap::VecSet;

    #[test]
    fn test_finder() {
        let example = vec![0, 7, 0, -7, 0, 2, 0, -1, 0, -1, 0, -1, 0, -2, 0, 1, 0, -2, 0, 1, 0, 1, 0, 0, 0, 0, -1, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0, -1, 0, -1, 0, 7, 0, -7, 0, -1, 0, -2, 0, 1, 0, -2, 0, 1, 0, 1, 0, 0, 0, -1, 0, -1, 0, 1, 0, 1, 0, -2, 0];
        let subs = find_maximal_repeated_subs(&example);
        let expected: VecSet<Subsequences> = [(15, vec![10, 46]), (7, vec![4, 36]),
            (6, vec![20, 27]), (5, vec![0, 42]), (5, vec![8, 60]), (5, vec![23, 58]),
            (5, vec![25, 62]), (5, vec![30, 34]), (1, vec![68, 70])
        ].iter().map(|(length, starts)| Subsequences {length: *length, starts: starts.clone()}).collect();

        for sub in subs.iter() {
            println!("{sub:?}");
            assert!(expected.contains(&sub));
        }
    }
}