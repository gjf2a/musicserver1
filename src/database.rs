use std::fmt::{Display, Formatter};
use enum_iterator::Sequence;
use crate::{MidiByte,Melody,Note};
use sqlite::{State,Connection};

const DATABASE_FILENAME: &str = "replayer_variations.db";

fn get_connection() -> Connection {
    let connection = sqlite::open(DATABASE_FILENAME).unwrap();
    connection.execute("CREATE TABLE IF NOT EXISTS main_table (timestamp INTEGER, rating TEXT, tag TEXT, source INTEGER);").unwrap();
    connection.execute("CREATE TABLE IF NOT EXISTS melodies (row INTEGER, pitch INTEGER, duration FLOAT, velocity INTEGER);").unwrap();
    connection
}

pub fn store_melody(melody: &Melody, source: Option<i64>, timestamp: i64) -> i64 {
    let connection = get_connection();
    connection
        .prepare("INSERT INTO main_table (timestamp, rating, source) VALUES (?, ?, ?)").unwrap()
        .bind(1, timestamp).unwrap()
        .bind(2, Preference::Neutral.to_string().as_str()).unwrap()
        .bind(3, source).unwrap()
        .next().unwrap();
    let mut statement = connection.prepare("SELECT last_insert_rowid()").unwrap();
    statement.next().unwrap();
    let row_id = statement.read::<i64>(0).unwrap();
    for note in melody.iter() {
        connection
            .prepare("INSERT INTO melodies (row, pitch, duration, velocity) VALUES (?, ?, ?, ?);").unwrap()
            .bind(1, row_id).unwrap()
            .bind(2, note.pitch() as i64).unwrap()
            .bind(3, note.duration()).unwrap()
            .bind(4, note.velocity() as i64).unwrap()
            .next().unwrap();
    }
    row_id
}

pub fn get_main_melody_ids() -> Vec<(i64, i64)> {
    let connection = get_connection();
    let mut statement = connection.prepare("SELECT rowid, timestamp FROM main_table WHERE source IS NULL").unwrap();
    let mut result = Vec::new();
    while let State::Row = statement.next().unwrap() {
        let row_id = statement.read::<i64>(0).unwrap();
        let timestamp = statement.read::<i64>(1).unwrap();
        result.push((row_id, timestamp));
    }
    result
}

pub fn get_variations_of(main_id: i64) -> Vec<(i64,i64)> {
    let connection = get_connection();
    let mut statement = connection
        .prepare("SELECT rowid, timestamp FROM main_table WHERE source = ?").unwrap()
        .bind(1, main_id).unwrap();
    let mut result = Vec::new();
    while let State::Row = statement.next().unwrap() {
        let row_id = statement.read::<i64>(0).unwrap();
        let timestamp = statement.read::<i64>(1).unwrap();
        result.push((row_id, timestamp));
    }
    result
}

pub fn get_melody(row_id: i64) -> Melody {
    let connection = get_connection();
    let mut statement = connection
        .prepare("SELECT pitch, duration, velocity from melodies WHERE row = ?").unwrap()
        .bind(1, row_id).unwrap();
    let mut melody = Melody::new();
    while let State::Row = statement.next().unwrap() {
        let pitch = statement.read::<i64>(0).unwrap();
        let duration = statement.read::<f64>(1).unwrap();
        let velocity = statement.read::<i64>(2).unwrap();
        let note = Note::new(pitch as MidiByte, duration, velocity as MidiByte);
        melody.add(note);
    }
    melody
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