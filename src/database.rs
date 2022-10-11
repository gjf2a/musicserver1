use std::{fmt::{Display, Formatter}, sync::{Arc, Mutex}};
use enum_iterator::Sequence;
use crate::{MidiByte,Melody,Note};
use sqlite::{State,Connection};
use anyhow::bail;
use chrono::{Utc, NaiveDate, NaiveTime, Local, TimeZone};
use std::str::FromStr;

const DATABASE_FILENAME: &str = "replayer_variations.db";

#[derive(Clone, Debug)]
pub struct Database {
    filename: String
}

impl Database {
    pub fn new() -> Arc<Mutex<Self>> {
        Arc::new(Mutex::new(Database {filename: DATABASE_FILENAME.to_string()}))
    }

    pub fn add_melody_and_variation(&mut self, melody: &Melody, variation: &Melody) {
        let player_info = MelodyInfo::new(self.clone(), melody, None);
        MelodyInfo::new(self.clone(), variation, Some(player_info.get_row_id()));
    }

    fn get_connection(&self) -> Connection {
        let connection = sqlite::open(self.filename.as_str()).unwrap();
        connection.execute("CREATE TABLE IF NOT EXISTS main_table (timestamp INTEGER, rating TEXT, tag TEXT, source INTEGER);").unwrap();
        connection.execute("CREATE TABLE IF NOT EXISTS melodies (row INTEGER, pitch INTEGER, duration FLOAT, velocity INTEGER);").unwrap();
        connection
    }
}

#[derive(Clone, Debug)]
pub struct MelodyInfo {
    timestamp: i64,
    row_id: i64,
    rating: Preference,
    source: Option<i64>,
    tag: String,
    melody: Option<Melody>,
    database: Arc<Mutex<Database>>
}

impl MelodyInfo {
    fn new(database: Arc<Mutex<Database>>, melody: &Melody, source: Option<i64>) -> Self {
        let timestamp = Utc::now().timestamp();
        let rating = Preference::Neutral;
        let dbase = database.clone();
        let dbase = dbase.lock().unwrap();
        let connection = dbase.get_connection();
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

        MelodyInfo {row_id, timestamp, melody: Some(melody.clone()), tag: tag.to_string(), source, rating, database }
    }

    pub fn get_main_melodies(database: Arc<Mutex<Database>>) -> Vec<Self> {
        let dbase = database.lock().unwrap();
        let connection = dbase.get_connection();
        let mut statement = connection.prepare("SELECT timestamp, rowid, tag, rating FROM main_table WHERE source IS NULL").unwrap();
        let mut result = Vec::new();
        while let State::Row = statement.next().unwrap() {
            let timestamp = statement.read::<i64>(0).unwrap();
            let row_id = statement.read::<i64>(1).unwrap();
            let tag = statement.read::<String>(2).unwrap();
            let rating = statement.read::<String>(3).unwrap().parse::<Preference>().unwrap();
            result.push(MelodyInfo {row_id, timestamp, melody: None, tag, source: None, rating, database: database.clone()});
        }
        result
    }

    pub fn get_variations_of(&self) -> Vec<Self> {
        let database = self.database.lock().unwrap();
        let connection = database.get_connection();
        let mut statement = connection
            .prepare("SELECT timestamp, rowid, tag, rating FROM main_table WHERE source = ?").unwrap()
            .bind(1, self.row_id).unwrap();
        let mut result = Vec::new();
        while let State::Row = statement.next().unwrap() {
            let timestamp = statement.read::<i64>(0).unwrap();
            let row_id = statement.read::<i64>(1).unwrap();
            let tag = statement.read::<String>(2).unwrap();
            let rating = statement.read::<String>(3).unwrap().parse::<Preference>().unwrap();
            result.push(MelodyInfo {row_id, timestamp, melody: None, tag, source: Some(self.row_id), rating, database: self.database.clone()});
        }
        result
    }

    pub fn get_melody(&mut self) -> Melody {
        if let Some(melody) = &self.melody {
            melody.clone()
        } else {
            let melody = get_melody(self.database.clone(), self.row_id);
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

    pub fn get_rating(&self) -> Preference {
        self.rating
    }

    pub fn update_rating(&mut self, new_rating: Preference) {
        if self.rating != new_rating {
            self.rating = new_rating;
            let new_rating = new_rating.to_string();
            let database = self.database.lock().unwrap();
            let connection = database.get_connection();
            connection
                .prepare("UPDATE main_table SET rating = ? WHERE rowid = ?").unwrap()
                .bind(1, new_rating.as_str()).unwrap()
                .bind(2, self.row_id).unwrap()
                .next().unwrap();
        }
    }
}

pub fn get_melody(database: Arc<Mutex<Database>>, row_id: i64) -> Melody {
    let database = database.lock().unwrap();
    let connection = database.get_connection();
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