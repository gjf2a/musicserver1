use std::fmt::{Display, Formatter};
use enum_iterator::Sequence;
use crate::Melody;
use sqlite::Connection;

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
        .prepare(format!("INSERT INTO main_table (timestamp, rating, source) VALUES (?, '{}', ?)", Preference::Neutral.to_string())).unwrap()
        .bind(1, timestamp).unwrap()
        .bind(2, source).unwrap();
    let mut statement = connection.prepare("SELECT last_insert_rowid()").unwrap();
    statement.next().unwrap();
    let row_id = statement.read::<i64>(0).unwrap();
    for note in melody.iter() {
        connection
            .prepare("INSERT INTO melodies (row, pitch, duration, velocity) VALUES (?, ?, ?, ?);").unwrap()
            .bind(1, row_id).unwrap()
            .bind(2, note.pitch() as i64).unwrap()
            .bind(3, note.duration()).unwrap()
            .bind(4, note.velocity() as i64).unwrap();
    }
    row_id
}
/*
pub fn get_last_melodies() -> ((Melody, u64), Option<(Melody, u64)>) {
    let row_id = {
        let connection = get_connection();
        let mut statement = connection.prepare("SELECT COUNT(*) from main_table;").unwrap();
        statement.read::<u64>(0).unwrap()
    };
    let (m1, m2) = get_melodies(row_id);
    ((m1, row_id), m2)
}

pub fn get_melodies(row_id: u64) -> (Melody, Option<(Melody, u64)>) {
    let connection = get_connection();
    let mut statement = connection
        .prepare("SELECT timestamp, source FROM main_table WHERE row_id = ?")
        .unwrap()
        .bind(1, row_id)
        .unwrap();

}

 */

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