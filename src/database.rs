use std::fmt::{Display, Formatter};
use enum_iterator::Sequence;
use crate::{MidiByte,Melody,Note};
use sqlite::{State,Connection};
use anyhow::bail;
use chrono::{Utc, NaiveDate, NaiveTime, Local, TimeZone};
use std::str::FromStr;

const DATABASE_FILENAME: &str = "replayer_variations.db";

fn get_connection() -> Connection {
    let connection = sqlite::open(DATABASE_FILENAME).unwrap();
    connection.execute("CREATE TABLE IF NOT EXISTS main_table (timestamp INTEGER, rating TEXT, tag TEXT, source INTEGER);").unwrap();
    connection.execute("CREATE TABLE IF NOT EXISTS melodies (row INTEGER, pitch INTEGER, duration FLOAT, velocity INTEGER);").unwrap();
    connection
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct MelodyInfo {
    timestamp: i64,
    row_id: i64,
    rating: Preference,
    source: Option<i64>,
    tag: String,
    melody: Option<Melody>
}

impl MelodyInfo {
    pub fn new(melody: &Melody, source: Option<i64>) -> Self {
        println!("MelodyInfo::new()");
        let timestamp = Utc::now().timestamp();
        let rating = Preference::Neutral;
        let connection = get_connection();
        let tag = "";
        connection
            .prepare("INSERT INTO main_table (timestamp, rating, source, tag) VALUES (?, ?, ?, ?)").unwrap()
            .bind(1, timestamp).unwrap()
            .bind(2, rating.to_string().as_str()).unwrap()
            .bind(3, source).unwrap()
            .bind(4, tag).unwrap()
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
        println!("quitting MelodyInfo::new()");

        MelodyInfo {row_id, timestamp, melody: Some(melody.clone()), tag: tag.to_string(), source, rating}
    }

    pub fn get_main_melodies() -> Vec<Self> {
        println!("get_main_melodies");
        let connection = get_connection();
        let mut statement = connection.prepare("SELECT timestamp, rowid, tag, rating FROM main_table WHERE source IS NULL").unwrap();
        let mut result = Vec::new();
        while let State::Row = statement.next().unwrap() {
            let timestamp = statement.read::<i64>(0).unwrap();
            let row_id = statement.read::<i64>(1).unwrap();
            let tag = statement.read::<String>(2).unwrap();
            let rating = statement.read::<String>(3).unwrap().parse::<Preference>().unwrap();
            result.push(MelodyInfo {row_id, timestamp, melody: None, tag, source: None, rating});
        }
        println!("quitting get_main_melodies");
        result
    }

    pub fn get_variations_of(&self) -> Vec<Self> {
        println!("get_variations_of");
        let connection = get_connection();
        let mut statement = connection
            .prepare("SELECT timestamp, rowid, tag, rating FROM main_table WHERE source = ?").unwrap()
            .bind(1, self.row_id).unwrap();
        let mut result = Vec::new();
        while let State::Row = statement.next().unwrap() {
            let timestamp = statement.read::<i64>(0).unwrap();
            let row_id = statement.read::<i64>(1).unwrap();
            let tag = statement.read::<String>(2).unwrap();
            let rating = statement.read::<String>(3).unwrap().parse::<Preference>().unwrap();
            result.push(MelodyInfo {row_id, timestamp, melody: None, tag, source: Some(self.row_id), rating});
        }
        println!("quitting get_variations_of");
        result
    }

    pub fn get_melody(&mut self) -> Melody {
        if let Some(melody) = &self.melody {
            melody.clone()
        } else {
            let melody = get_melody(self.row_id);
            self.melody = Some(melody.clone());
            melody
        }
    }

    pub fn get_row_id(&self) -> i64 {
        self.row_id
    }

    pub fn get_date(&self) -> NaiveDate {
        Local.timestamp(self.timestamp, 0).date_naive()
    }

    pub fn get_time(&self) -> NaiveTime {
        Local.timestamp(self.timestamp, 0).time()
    }
}

pub fn get_melody(row_id: i64) -> Melody {
    println!("get_melody");
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
    println!("quitting get_melody");
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

impl FromStr for Preference {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Favorite" => Ok(Preference::Favorite),
            "Neutral" => Ok(Preference::Neutral),
            "Ignore" => Ok(Preference::Ignore),
            _ => bail!("No match for {s}")
        }
    }
}