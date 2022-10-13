use std::{fmt::{Display, Formatter}, sync::{Arc, Mutex}};
use enum_iterator::Sequence;
use crate::{MidiByte,Melody,Note};
use sqlite::{State,Connection};
use anyhow::bail;
use chrono::{Utc, NaiveDate, NaiveTime, Local, TimeZone};
use std::str::FromStr;
use std::collections::BTreeMap;
use crossbeam_queue::SegQueue;

#[derive(Clone, Debug)]
pub struct FromAiMsg {
    pub melody: Melody,
    pub variation: Melody
}

pub fn start_database_thread(
    dbase2gui: Arc<SegQueue<(MelodyInfo,MelodyInfo)>>,
    gui2dbase: Arc<SegQueue<MelodyInfo>>,
    ai2dbase: Arc<SegQueue<FromAiMsg>>,
    mut database: Database
) {
    std::thread::spawn(move || {
        loop {
            if let Some(info) = gui2dbase.pop() {
                database.update_rating(info.get_row_id(), info.get_rating()).unwrap();
            }

            if let Some(msg) = ai2dbase.pop() {
                let info = database.add_melody_and_variation(&msg.melody, &msg.variation).unwrap();
                dbase2gui.push(info);
            }
        }
    });
}


const DATABASE_FILENAME: &str = "replayer_variations.db";

#[derive(Clone, Debug)]
pub struct Database {
    filename: String,
    melodies: BTreeMap<i64, MelodyInfo>,
    variations: BTreeMap<i64, MelodyInfo>,
    melody2variations: BTreeMap<i64, Vec<i64>>,
    melody_cache: BTreeMap<i64, Melody>
}

/*

// A new database schema to consider:

    fn get_connection(&self) -> anyhow::Result<Connection> {
        let connection = sqlite::open(self.filename.as_str()).unwrap();
        connection.execute("CREATE TABLE IF NOT EXISTS melody_index (timestamp INTEGER, rating TEXT, tag TEXT);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS melody_variation (melody_row INTEGER, variation_row INTEGER);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS melodies (row INTEGER, pitch INTEGER, duration FLOAT, velocity INTEGER);")?;
        Ok(connection)
    }
// Good overview of indexing in SQLite: https://medium.com/@JasonWyatt/squeezing-performance-from-sqlite-indexes-indexes-c4e175f3c346

*/

impl Database {

    fn get_connection(&self) -> anyhow::Result<Connection> {
        let connection = sqlite::open(self.filename.as_str()).unwrap();
        connection.execute("CREATE TABLE IF NOT EXISTS main_table (timestamp INTEGER, rating TEXT, tag TEXT, source INTEGER);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS melodies (row INTEGER, pitch INTEGER, duration FLOAT, velocity INTEGER);")?;
        Ok(connection)
    }

    pub fn new() -> anyhow::Result<Self> {
        let mut database = Database {filename: DATABASE_FILENAME.to_string(), melodies: BTreeMap::new(), variations: BTreeMap::new(), melody_cache: BTreeMap::new(), melody2variations: BTreeMap::new()};
        let connection = database.get_connection()?;
        let mut statement = connection.prepare("SELECT rowid, timestamp, rating, tag, source FROM main_table")?;
        while let State::Row = statement.next().unwrap() {
            let rowid = statement.read::<i64>(0)?;
            let timestamp = statement.read::<i64>(1)?;
            let rating = statement.read::<String>(2)?.parse::<Preference>()?;
            let tag = statement.read::<String>(3)?;
            let source = statement.read::<Option<i64>>(4)?;
            let info = MelodyInfo {timestamp, rowid, rating, source, tag};
            database.add_info(info.clone());
        }
        Ok(database)
    }

    fn add_info(&mut self, info: MelodyInfo) {
        (if let Some(source) = info.source {
            self.add_variation(source, info.rowid);
            &mut self.variations
        } else {
            &mut self.melodies
        }).insert(info.rowid, info);
    }

    fn add_variation(&mut self, melody_row: i64, variation_row: i64) {
        match self.melody2variations.get_mut(&melody_row) {
            None => {self.melody2variations.insert(melody_row, vec![variation_row]);}
            Some(vars) => {vars.push(variation_row);}
        };
    }

    pub fn num_melodies(&self) -> usize {
        self.melodies.len()
    }

    pub fn num_variations_of(&self, rowid: i64) -> usize {
        self.melody2variations.get(&rowid).map_or(0, |v| v.len())
    }

    pub fn melodies(&self) -> Vec<MelodyInfo> {
        self.melodies.values().cloned().collect()
    }

    pub fn variations_of(&self, rowid: i64) -> Vec<MelodyInfo> {
        self.melody2variations.get(&rowid).map_or(vec![], |v| {
            v.iter().map(|vid| self.variations.get(vid).unwrap().clone()).collect()
        })
    }

    pub fn info(&self, rowid: i64) -> Option<MelodyInfo> {
        self.melodies.get(&rowid)
            .or(self.variations.get(&rowid))
            .map(|info| info.clone())
    }

    pub fn melody(&mut self, rowid: i64) -> anyhow::Result<Melody> {
        let cached = self.melody_cache.get(&rowid);
        if cached.is_some() {
            Ok(cached.map(|m| m.clone()).unwrap())
        } else {
            let connection = self.get_connection()?;
            let mut statement = connection
                .prepare("SELECT pitch, duration, velocity from melodies WHERE row = ?")?
                .bind(1, rowid)?;
            let mut melody = Melody::new();
            while let State::Row = statement.next()? {
                let pitch = statement.read::<i64>(0)?;
                let duration = statement.read::<f64>(1)?;
                let velocity = statement.read::<i64>(2)?;
                let note = Note::new(pitch as MidiByte, duration, velocity as MidiByte);
                melody.add(note);
            }
            self.melody_cache.insert(rowid, melody.clone());
            Ok(melody)
        }
    }

    pub fn add_melody_and_variation(&mut self, melody: &Melody, variation: &Melody) -> anyhow::Result<(MelodyInfo, MelodyInfo)> {
        let player_info = self.store_melody(melody, None)?;
        let variation_info = self.store_melody(variation, Some(player_info.rowid))?;
        Ok((player_info, variation_info))
    }

    pub fn update_rating(&mut self, rowid: i64, new_rating: Preference) -> anyhow::Result<()> {
        let mut row_info = self.melodies.get(&rowid).or(self.variations.get(&rowid)).unwrap().clone();
        if row_info.rating != new_rating {
            row_info.rating = new_rating;
            let new_rating = new_rating.to_string();
            let connection = self.get_connection()?;
            connection
                .prepare("UPDATE main_table SET rating = ? WHERE rowid = ?").unwrap()
                .bind(1, new_rating.as_str()).unwrap()
                .bind(2, row_info.rowid).unwrap()
                .next().unwrap();
            self.add_info(row_info);
        }
        Ok(())
    }

    fn store_melody(&mut self, melody: &Melody, source: Option<i64>) -> anyhow::Result<MelodyInfo> {
        let timestamp = Utc::now().timestamp();
        let rating = Preference::Neutral;
        let connection = self.get_connection()?;
        let tag = "";
        connection
            .prepare("INSERT INTO main_table (timestamp, rating, source, tag) VALUES (?, ?, ?, ?)")?
            .bind(1, timestamp)?
            .bind(2, rating.to_string().as_str())?
            .bind(3, source)?
            .bind(4, tag)?
            .next()?;
        let mut statement = connection.prepare("SELECT last_insert_rowid()")?;
        statement.next()?;
        let rowid = statement.read::<i64>(0)?;

        for note in melody.iter() {
            connection
                .prepare("INSERT INTO melodies (row, pitch, duration, velocity) VALUES (?, ?, ?, ?);").unwrap()
                .bind(1, rowid).unwrap()
                .bind(2, note.pitch() as i64).unwrap()
                .bind(3, note.duration()).unwrap()
                .bind(4, note.velocity() as i64).unwrap()
                .next().unwrap();
        }

        let info = MelodyInfo { rowid, timestamp, tag: tag.to_string(), source, rating };
        self.add_info(info.clone());
        self.melody_cache.insert(info.rowid, melody.clone());
        Ok(info)
    }
}

#[derive(Clone, Debug)]
pub struct MelodyInfo {
    timestamp: i64,
    rowid: i64,
    rating: Preference,
    source: Option<i64>,
    tag: String
}

impl MelodyInfo {
    pub fn get_row_id(&self) -> i64 {
        self.rowid
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

    pub fn set_rating(&mut self, pref: Preference) {
        self.rating = pref;
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