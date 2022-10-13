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
                database.update_info(&info).unwrap();
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
    melody_cache: BTreeMap<i64, Melody>
}

impl Database {
    fn get_connection(&self) -> anyhow::Result<Connection> {
        // Good overview of indexing in SQLite: https://medium.com/@JasonWyatt/squeezing-performance-from-sqlite-indexes-indexes-c4e175f3c346
        let connection = sqlite::open(self.filename.as_str()).unwrap();
        connection.execute("CREATE TABLE IF NOT EXISTS melody_index (timestamp INTEGER, rating TEXT, tag TEXT);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS melody_variation (melody_row INTEGER, variation_row INTEGER);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS melodies (row INTEGER, pitch INTEGER, duration FLOAT, velocity INTEGER);")?;
        Ok(connection)
    }

    pub fn new() -> Self {
        Database {filename: DATABASE_FILENAME.to_string(), melody_cache: BTreeMap::new()}
    }

    pub fn get_melody_pairs(&self) -> anyhow::Result<Vec<(MelodyInfo,MelodyInfo)>> {
        let mut result = vec![];
        let connection = database.get_connection()?;
        let mut statement = connection.prepare("SELECT melody_row, variation_row FROM melody_variation")?;
        while let State::Row = statement.next().unwrap() {
            let melody_row = statement.read::<i64>(0)?;
            let variation_row = statement.read::<i64>(1)?;
            result.push((Self::info_for(&connection, melody_row)?, Self::info_for(&connection, variation_row)?));
        }
        Ok(result)
    }

    fn info_for(connection: &Connection, rowid: i64) -> anyhow::Result<MelodyInfo> {
        let mut statement = connection.prepare("SELECT rowid, timestamp, rating, tag FROM melody_index WHERE rowid = ?")?.bind(1, rowid)?;
        if let State::Row = statement.next()? {
            let timestamp = statement.read::<i64>(0)?;
            let rating = statement.read::<String>(1)?.parse::<Preference>()?;
            let tag = statement.read::<String>(2)?;
            Ok(MelodyInfo {rowid, timestamp, rating, tag})
        } else {
            bail!("{rowid} not in database.")
        }
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

    pub fn update_info(&mut self, new_info: &MelodyInfo) -> anyhow::Result<()> {
        let connection = self.get_connection()?;
        connection.prepare("UPDATE melody_index SET timestamp = ?, rating = ?, tag = ? WHERE rowid = ?")?
            .bind(1, new_info.timestamp)?
            .bind(2, new_info.rating.to_string().as_str())?
            .bind(3, new_info.tag.as_str())?
            .next()?;
        Ok(())
    }

    pub fn add_melody_and_variation(&mut self, melody: &Melody, variation: &Melody) -> anyhow::Result<(MelodyInfo, MelodyInfo)> {
        let player_info = self.store_melody(melody)?;
        let variation_info = self.store_melody(variation)?;
        let connection = self.get_connection()?;
        connection.prepare("INSERT INTO melody_variation (melody_row, variation_row) VALUES (?,?)")?
            .bind(1, player_info.rowid)?
            .bind(2, variation_info.rowid)?.next().unwrap();
        Ok((player_info, variation_info))
    }

    fn store_melody(&mut self, melody: &Melody) -> anyhow::Result<MelodyInfo> {
        let timestamp = Utc::now().timestamp();
        let rating = Preference::Neutral;
        let connection = self.get_connection()?;
        let tag = "";
        connection
            .prepare("INSERT INTO melody_index (timestamp, rating, tag) VALUES (?, ?, ?)")?
            .bind(1, timestamp)?
            .bind(2, rating.to_string().as_str())?
            .bind(3, tag)?
            .next().unwrap();
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

        let info = MelodyInfo { rowid, timestamp, tag: tag.to_string(), rating };
        self.add_info(info.clone());
        self.melody_cache.insert(info.rowid, melody.clone());
        Ok(info)
    }
}

#[derive(Clone, Debug)]
pub struct MelodyInfo {
    rowid: i64,
    timestamp: i64,
    rating: Preference,
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