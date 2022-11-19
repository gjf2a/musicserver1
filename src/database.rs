use crate::analyzer::{Melody, MidiByte, Note};
use anyhow::bail;
use chrono::{Local, NaiveDate, NaiveTime, TimeZone, Utc};
use crossbeam_queue::SegQueue;
use enum_iterator::Sequence;
use sqlite::{Connection, State};
use std::collections::BTreeMap;
use std::str::FromStr;
use std::{
    fmt::{Display, Formatter},
    sync::Arc,
};

#[derive(Clone, Debug)]
pub struct FromAiMsg {
    pub melody: Melody,
    pub variation: Melody,
}

#[derive(Clone, Debug)]
pub struct GuiDatabaseUpdate {
    pub rowid: i64,
    pub tag: String,
    pub rating: Preference,
}

pub fn start_database_thread(
    dbase2gui: Arc<SegQueue<(MelodyInfo, MelodyInfo)>>,
    gui2dbase: Arc<SegQueue<GuiDatabaseUpdate>>,
    ai2dbase: Arc<SegQueue<FromAiMsg>>,
    mut database: Database,
) {
    std::thread::spawn(move || loop {
        if let Some(info) = gui2dbase.pop() {
            database.update_info(&info).unwrap();
        }

        if let Some(msg) = ai2dbase.pop() {
            let info = database
                .add_melody_and_variation(&msg.melody, &msg.variation)
                .unwrap();
            dbase2gui.push(info);
        }
    });
}

const DATABASE_FILENAME: &str = "replayer_variations.db";

#[derive(Clone, Debug)]
pub struct Database {
    filename: String,
    melody_cache: BTreeMap<i64, Melody>,
}

impl Database {
    fn get_connection(&self) -> anyhow::Result<Connection> {
        // Good overview of indexing in SQLite: https://medium.com/@JasonWyatt/squeezing-performance-from-sqlite-indexes-indexes-c4e175f3c346
        let connection = sqlite::open(self.filename.as_str()).unwrap();
        connection.execute("CREATE TABLE IF NOT EXISTS melody_index (timestamp INTEGER, rating TEXT, tag TEXT, scale_name TEXT);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS melody_variation (melody_row INTEGER, variation_row INTEGER);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS melodies (row INTEGER, pitch INTEGER, duration FLOAT, velocity INTEGER);")?;
        Ok(connection)
    }

    pub fn new() -> Self {
        Database {
            filename: DATABASE_FILENAME.to_string(),
            melody_cache: BTreeMap::new(),
        }
    }

    pub fn get_melody_pairs(&mut self) -> anyhow::Result<Vec<(MelodyInfo, MelodyInfo)>> {
        let mut result = vec![];
        let connection = self.get_connection()?;
        let mut statement =
            connection.prepare("SELECT melody_row, variation_row FROM melody_variation")?;
        while let State::Row = statement.next().unwrap() {
            let melody_row = statement.read::<i64, usize>(0)?;
            let variation_row = statement.read::<i64, usize>(1)?;
            let melody = self.melody(&connection, melody_row)?;
            let variation = self.melody(&connection, variation_row)?;
            result.push((
                Self::info_for(&connection, melody_row, melody)?,
                Self::info_for(&connection, variation_row, variation)?,
            ));
        }
        Ok(result)
    }

    fn info_for(connection: &Connection, rowid: i64, melody: Melody) -> anyhow::Result<MelodyInfo> {
        let mut statement = connection.prepare(
            "SELECT rowid, timestamp, rating, tag, scale_name FROM melody_index WHERE rowid = ?",
        )?;
        statement.bind((1, rowid))?;
        if let State::Row = statement.next()? {
            let timestamp = statement.read::<i64, usize>(1)?;
            let rating = statement.read::<String, usize>(2)?.parse::<Preference>()?;
            let tag = statement.read::<String, usize>(3)?;
            let scale_name = statement.read::<String, usize>(4)?;
            Ok(MelodyInfo {
                rowid,
                timestamp,
                rating,
                tag,
                scale_name,
                melody,
            })
        } else {
            bail!("{rowid} not in database.")
        }
    }

    pub fn melody(&mut self, connection: &Connection, rowid: i64) -> anyhow::Result<Melody> {
        let cached = self.melody_cache.get(&rowid);
        if cached.is_some() {
            Ok(cached.map(|m| m.clone()).unwrap())
        } else {
            let mut statement = connection
                .prepare("SELECT pitch, duration, velocity from melodies WHERE row = ?")?;
            statement.bind((1, rowid))?;
            let mut melody = Melody::new();
            while let State::Row = statement.next()? {
                let pitch = statement.read::<i64, usize>(0)?;
                let duration = statement.read::<f64, usize>(1)?;
                let velocity = statement.read::<i64, usize>(2)?;
                let note = Note::new(pitch as MidiByte, duration, velocity as MidiByte);
                melody.add(note);
            }
            self.melody_cache.insert(rowid, melody.clone());
            Ok(melody)
        }
    }

    pub fn update_info(&mut self, new_info: &GuiDatabaseUpdate) -> anyhow::Result<()> {
        let connection = self.get_connection()?;
        let mut statement =
            connection.prepare("UPDATE melody_index SET rating = ?, tag = ? WHERE rowid = ?")?;
        statement.bind((1, new_info.rating.to_string().as_str()))?;
        statement.bind((2, new_info.tag.as_str()))?;
        statement.bind((3, new_info.rowid))?;
        statement.next()?;
        Ok(())
    }

    pub fn add_melody_and_variation(
        &mut self,
        melody: &Melody,
        variation: &Melody,
    ) -> anyhow::Result<(MelodyInfo, MelodyInfo)> {
        let player_info = self.store_melody(melody)?;
        let variation_info = self.store_melody(variation)?;
        let connection = self.get_connection()?;
        let mut statement = connection
            .prepare("INSERT INTO melody_variation (melody_row, variation_row) VALUES (?,?)")?;
        statement.bind((1, player_info.rowid))?;
        statement.bind((2, variation_info.rowid))?;
        statement.next().unwrap();
        Ok((player_info, variation_info))
    }

    fn store_melody(&mut self, melody: &Melody) -> anyhow::Result<MelodyInfo> {
        let timestamp = Utc::now().timestamp();
        let scale = melody.best_scale_for();
        let rating = Preference::Neutral;
        let connection = self.get_connection()?;
        let tag = "";
        let mut statement = connection.prepare(
            "INSERT INTO melody_index (timestamp, rating, tag, scale_name) VALUES (?, ?, ?, ?)",
        )?;
        statement.bind((1, timestamp))?;
        statement.bind((2, rating.to_string().as_str()))?;
        statement.bind((3, tag))?;
        statement.bind((4, scale.name().as_str()))?;
        statement.next().unwrap();
        let mut statement = connection.prepare("SELECT last_insert_rowid()")?;
        statement.next()?;
        let rowid = statement.read::<i64, usize>(0)?;

        for note in melody.iter() {
            let mut statement = connection
                .prepare(
                    "INSERT INTO melodies (row, pitch, duration, velocity) VALUES (?, ?, ?, ?);",
                )
                .unwrap();
            statement.bind((1, rowid))?;
            statement.bind((2, note.pitch() as i64))?;
            statement.bind((3, note.duration()))?;
            statement.bind((4, note.velocity() as i64))?;
            statement.next().unwrap();
        }

        let info = MelodyInfo {
            rowid,
            timestamp,
            tag: tag.to_string(),
            rating,
            scale_name: scale.name(),
            melody: melody.clone(),
        };
        self.melody_cache.insert(info.rowid, melody.clone());
        Ok(info)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MelodyInfo {
    rowid: i64,
    timestamp: i64,
    rating: Preference,
    tag: String,
    scale_name: String,
    melody: Melody,
}

impl MelodyInfo {
    pub fn get_row_id(&self) -> i64 {
        self.rowid
    }

    pub fn get_date(&self) -> NaiveDate {
        Local.timestamp_opt(self.timestamp, 0).unwrap().date_naive()
    }

    pub fn get_time(&self) -> NaiveTime {
        Local.timestamp_opt(self.timestamp, 0).unwrap().time()
    }

    pub fn date_time_stamp(&self) -> String {
        format!("{:?} {:?}", self.get_date(), self.get_time())
    }

    pub fn get_scale_name(&self) -> String {
        self.scale_name.clone()
    }

    pub fn rating(&self) -> Preference {
        self.rating
    }

    pub fn set_rating(&mut self, pref: Preference) {
        self.rating = pref;
    }

    pub fn get_update(&self) -> GuiDatabaseUpdate {
        GuiDatabaseUpdate {
            rowid: self.rowid,
            tag: self.tag.clone(),
            rating: self.rating,
        }
    }

    pub fn melody(&self) -> &Melody {
        &self.melody
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Sequence, Debug)]
pub enum Preference {
    Favorite,
    Neutral,
    Ignore,
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
            _ => bail!("No match for {s}"),
        }
    }
}
