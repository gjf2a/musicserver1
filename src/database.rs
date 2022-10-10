use std::fmt::{Display, Formatter};
use enum_iterator::Sequence;
use crate::Melody;

// Current thinking:
// * Open/close connections on each operation
// * Do I even need the Database struct, or should I just write some functions?
//   * struct: I could store the sqlite filename in there
//   * no struct: Just make the filename a constant!

pub struct Database {

}

impl Database {
    /*
    pub fn open() -> Self {

    }

     */

    // TODO: Needs a timestamp parameter for when it was created.
    pub fn store_human_melody(&mut self, human: &Melody) {

    }

    // TODO: Needs two timestamp parameters: when created, and reference back to the human source
    pub fn store_variation(&mut self, variation: &Melody) {

    }
}

#[derive(Copy, Clone, Eq, PartialEq, Sequence, Debug)]
pub enum Preference {
    Favorite,
    Neutral,
    Ignore
}

impl Display for Preference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}