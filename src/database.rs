use crate::analyzer::{Melody, MidiByte, Note};
use crate::chords::Chord;
use anyhow::bail;
use chrono::{Local, NaiveDate, NaiveTime, TimeZone, Utc};
use crossbeam_queue::SegQueue;
use enum_iterator::Sequence;
use sqlite::{Connection, State};
use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;
use std::{
    fmt::{Display, Formatter},
    sync::Arc,
};

#[derive(Clone, Debug)]
pub struct VariationStats {
    pub algorithm_name: String,
    pub random_prob: f64,
    pub ornament_prob: f64,
    pub min_note_duration: f64,
    pub whimsify: bool,
}

#[derive(Clone, Debug)]
pub enum FromAiMsg {
    MelodyOnly(Melody),
    MelodyVariation {
        melody: Melody,
        variation: Melody,
        stats: VariationStats,
    },
    AlternateVariation {
        melody_id: i64,
        variation: Melody,
        stats: VariationStats,
    },
}

#[derive(Clone, Debug)]
pub enum GuiDatabaseUpdate {
    Info {
        rowid: i64,
        rating: Preference,
    },
    NewTag {
        rowid: i64,
        tag: String,
    },
    VariationsOf(i64),
    RefreshAllMelodies {
        min_today_pref: Preference,
        min_older_pref: Preference,
    },
    RefreshAllPairs {
        min_today_pref: Preference,
        min_older_pref: Preference,
    },
}

#[derive(Clone, Debug)]
pub enum DatabaseGuiUpdate {
    Info {
        melody: MelodyInfo,
        variation: MelodyInfo,
        stats: VariationStats,
    },
    AllPairs(Vec<(MelodyInfo, MelodyInfo, VariationStats)>),
    Melodies(Vec<MelodyInfo>),
}

pub fn start_database_thread(
    dbase2gui: Arc<SegQueue<DatabaseGuiUpdate>>,
    gui2dbase: Arc<SegQueue<GuiDatabaseUpdate>>,
    ai2dbase: Arc<SegQueue<FromAiMsg>>,
    mut database: Database,
) {
    std::thread::spawn(move || loop {
        if let Some(info) = gui2dbase.pop() {
            match info {
                GuiDatabaseUpdate::VariationsOf(rowid) => {
                    let pairs = database.get_single_melody_variations(rowid).unwrap();
                    dbase2gui.push(DatabaseGuiUpdate::AllPairs(pairs));
                }
                GuiDatabaseUpdate::Info { rowid, rating } => {
                    database.update_info(rowid, rating).unwrap();
                }
                GuiDatabaseUpdate::NewTag { rowid, tag } => {
                    database.add_tag_for(rowid, tag).unwrap();
                }
                GuiDatabaseUpdate::RefreshAllPairs {
                    min_today_pref,
                    min_older_pref,
                } => {
                    let pairs = database
                        .get_melody_pairs(min_today_pref, min_older_pref)
                        .unwrap();
                    dbase2gui.push(DatabaseGuiUpdate::AllPairs(pairs));
                }
                GuiDatabaseUpdate::RefreshAllMelodies {
                    min_today_pref,
                    min_older_pref,
                } => {
                    let melodies = database
                        .get_melodies_only(min_today_pref, min_older_pref)
                        .unwrap();
                    dbase2gui.push(DatabaseGuiUpdate::Melodies(melodies));
                }
            }
        }

        if let Some(msg) = ai2dbase.pop() {
            match msg {
                FromAiMsg::MelodyOnly(_) => todo!("Not implemented yet"),
                FromAiMsg::MelodyVariation {
                    melody,
                    variation,
                    stats,
                } => {
                    let info = database
                        .add_melody_and_variation(&melody, &variation, &stats)
                        .unwrap();
                    dbase2gui.push(DatabaseGuiUpdate::Info {
                        melody: info.0,
                        variation: info.1,
                        stats,
                    });
                }
                FromAiMsg::AlternateVariation {
                    melody_id,
                    variation,
                    stats,
                } => {
                    let variation_info = database
                        .add_variation(melody_id, &variation, &stats)
                        .unwrap();
                    let melody_info = database.melody_and_info_for(melody_id).unwrap();
                    dbase2gui.push(DatabaseGuiUpdate::Info {
                        melody: melody_info,
                        variation: variation_info,
                        stats,
                    });
                }
            }
        }
    });
}

const DATABASE_FILENAME: &str = "taggable_variations.db";

#[derive(Clone, Debug)]
pub struct Database {
    filename: String,
    melody_cache: BTreeMap<i64, Melody>,
}

impl Database {
    fn get_connection(&self) -> anyhow::Result<Connection> {
        // Good overview of indexing in SQLite: https://medium.com/@JasonWyatt/squeezing-performance-from-sqlite-indexes-indexes-c4e175f3c346
        let connection = sqlite::open(self.filename.as_str()).unwrap();
        connection
            .execute("CREATE TABLE IF NOT EXISTS melody_index (timestamp INTEGER, rating TEXT);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS tags (melody_row INTEGER, tag TEXT);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS melodies (melody_row INTEGER, pitch INTEGER, duration FLOAT, velocity INTEGER);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS variation_info (variation_row INTEGER, original_row INTEGER, algorithm_name TEXT, random_prob FLOAT, ornament_prob FLOAT, min_note_duration FLOAT, whimsify INTEGER);")?;

        connection.execute(
            "CREATE TABLE IF NOT EXISTS progression_index (timestamp INTEGER, rating TEXT);",
        )?;
        connection.execute(
            "CREATE TABLE IF NOT EXISTS progression_tags (progression_row INTEGER, tag TEXT);",
        )?;
        connection.execute("CREATE TABLE IF NOT EXISTS chord_index (timestamp INTEGER);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS progression_times (progression_row INTEGER, chord_row INTEGER, start FLOAT, duration FLOAT);")?;
        connection.execute("CREATE TABLE IF NOT EXISTS chord_notes (chord_row INTEGER, pitch INTEGER, velocity INTEGER);")?;

        connection
            .execute("CREATE INDEX IF NOT EXISTS original_rows ON variation_info (original_row)")?;
        connection.execute(
            "CREATE INDEX IF NOT EXISTS variation_rows ON variation_info (variation_row)",
        )?;
        connection.execute("CREATE INDEX IF NOT EXISTS melodies_rows ON melodies (melody_row)")?;
        connection.execute("CREATE INDEX IF NOT EXISTS timestamps ON melody_index (timestamp)")?;
        connection.execute("CREATE INDEX IF NOT EXISTS ratings ON melody_index (rating)")?;
        connection.execute("CREATE INDEX IF NOT EXISTS melody_tags ON tags (melody_row)")?;
        connection.execute("CREATE INDEX IF NOT EXISTS tagged ON tags (tag)")?;
        connection.execute(
            "CREATE INDEX IF NOT EXISTS variation_algorithms ON variation_info (algorithm_name)",
        )?;

        connection.execute(
            "CREATE INDEX IF NOT EXISTS progression_rows ON progression_times (progression_row)",
        )?;
        connection.execute(
            "CREATE INDEX IF NOT EXISTS progression_timestamps ON progression_index (timestamp)",
        )?;
        connection.execute(
            "CREATE INDEX IF NOT EXISTS progression_ratings ON progression_index (rating)",
        )?;
        connection.execute(
            "CREATE INDEX IF NOT EXISTS progression_tag ON progression_tags (progression_row)",
        )?;
        connection
            .execute("CREATE INDEX IF NOT EXISTS progression_tagged ON progression_tags (tag)")?;
        Ok(connection)
    }

    pub fn tags_for(connection: &Connection, melody_id: i64) -> anyhow::Result<BTreeSet<String>> {
        let cmd = "SELECT tag FROM tags WHERE melody_row = ?";
        let mut statement = connection.prepare(cmd)?;
        statement.bind((1, melody_id))?;
        let mut result = BTreeSet::new();
        while let State::Row = statement.next()? {
            result.insert(statement.read::<String, usize>(0)?);
        }
        Ok(result)
    }

    pub fn add_tag_for(&self, melody_id: i64, tag: String) -> anyhow::Result<()> {
        let connection = self.get_connection()?;
        let mut statement =
            connection.prepare("INSERT INTO tags (melody_row, tag) VALUES (?, ?)")?;
        statement.bind((1, melody_id))?;
        statement.bind((2, tag.as_str()))?;
        statement.next()?;
        Ok(())
    }

    pub fn new() -> Self {
        Database {
            filename: DATABASE_FILENAME.to_string(),
            melody_cache: BTreeMap::new(),
        }
    }

    pub fn one_day_ago() -> i64 {
        Local::now().timestamp() - (24 * 60 * 60)
    }

    pub fn is_today(timestamp: i64) -> bool {
        timestamp > Self::one_day_ago()
    }

    pub fn get_melody_pairs(
        &mut self,
        min_today_pref: Preference,
        min_older_pref: Preference,
    ) -> anyhow::Result<Vec<(MelodyInfo, MelodyInfo, VariationStats)>> {
        let mut result = vec![];
        let connection = self.get_connection()?;
        let melody_variation_ids =
            Self::get_variation_info_ids(&connection, min_today_pref, min_older_pref)?;
        for (original_id, variation_id) in melody_variation_ids {
            let original = self.melody(&connection, original_id)?;
            let variation = self.melody(&connection, variation_id)?;
            let variation_info = Self::info_for(&connection, variation_id, variation)?;
            let original_info = Self::info_for(&connection, original_id, original)?;
            let stats = self.stats(&connection, variation_id)?;
            result.push((original_info, variation_info, stats));
        }
        Ok(result)
    }

    pub fn get_single_melody_variations(
        &mut self,
        melody_id: i64,
    ) -> anyhow::Result<Vec<(MelodyInfo, MelodyInfo, VariationStats)>> {
        let connection = self.get_connection()?;
        let original = self.melody(&connection, melody_id)?;
        let original_info = Self::info_for(&connection, melody_id, original)?;
        let cmd = "SELECT variation_row FROM variation_info WHERE original_row = ?";
        let mut statement = connection.prepare(cmd)?;
        statement.bind((1, melody_id))?;
        let mut result = vec![];
        while let State::Row = statement.next()? {
            let variation_id = statement.read::<i64, usize>(0)?;
            let variation = self.melody(&connection, variation_id)?;
            let variation_info = Self::info_for(&connection, variation_id, variation)?;
            let stats = self.stats(&connection, variation_id)?;
            result.push((original_info.clone(), variation_info, stats));
        }
        Ok(result)
    }

    pub fn get_melodies_only(
        &mut self,
        min_today_pref: Preference,
        min_older_pref: Preference,
    ) -> anyhow::Result<Vec<MelodyInfo>> {
        let mut result = vec![];
        let connection = self.get_connection()?;
        let melody_ids = Self::get_melody_ids(&connection, min_today_pref, min_older_pref)?;
        for melody_id in melody_ids {
            let melody = self.melody(&connection, melody_id)?;
            let melody_info = Self::info_for(&connection, melody_id, melody)?;
            result.push(melody_info);
        }
        Ok(result)
    }

    fn get_melody_ids(
        connection: &Connection,
        min_today_pref: Preference,
        min_older_pref: Preference,
    ) -> anyhow::Result<Vec<i64>> {
        let mut result = vec![];
        let cutoff = Self::one_day_ago();
        Self::add_melody_ids(connection, "<=", cutoff, min_older_pref, &mut result)?;
        Self::add_melody_ids(connection, ">", cutoff, min_today_pref, &mut result)?;
        Ok(result)
    }

    fn add_melody_ids(
        connection: &Connection,
        comparison: &str,
        cutoff: i64,
        min_pref: Preference,
        result: &mut Vec<i64>,
    ) -> anyhow::Result<()> {
        let template = "SELECT DISTINCT original_row FROM variation_info INNER JOIN melody_index ON variation_info.original_row = melody_index.rowid";
        let statement_str = format!(
            "{template} WHERE timestamp {comparison} ? AND {}",
            min_pref.sql_choice_str()
        );
        let mut statement = connection.prepare(statement_str)?;
        statement.bind((1, cutoff))?;
        while let State::Row = statement.next()? {
            let original_row = statement.read::<i64, usize>(0)?;
            result.push(original_row);
        }
        Ok(())
    }

    fn get_variation_info_ids(
        connection: &Connection,
        min_today_pref: Preference,
        min_older_pref: Preference,
    ) -> anyhow::Result<Vec<(i64, i64)>> {
        let mut result = vec![];
        let cutoff = Self::one_day_ago();
        Self::add_variation_info_ids(connection, "<=", cutoff, min_older_pref, &mut result)?;
        Self::add_variation_info_ids(connection, ">", cutoff, min_today_pref, &mut result)?;
        Ok(result)
    }

    fn add_variation_info_ids(
        connection: &Connection,
        comparison: &str,
        cutoff: i64,
        min_pref: Preference,
        result: &mut Vec<(i64, i64)>,
    ) -> anyhow::Result<()> {
        let template = "SELECT original_row, variation_row FROM variation_info INNER JOIN melody_index ON variation_info.variation_row = melody_index.rowid";
        let statement_str = format!(
            "{template} WHERE timestamp {comparison} ? AND {}",
            min_pref.sql_choice_str()
        );
        let mut statement = connection.prepare(statement_str)?;
        statement.bind((1, cutoff))?;
        while let State::Row = statement.next()? {
            let original_row = statement.read::<i64, usize>(0)?;
            let variation_row = statement.read::<i64, usize>(1)?;
            result.push((original_row, variation_row));
        }
        Ok(())
    }

    pub fn melody_and_info_for(&mut self, rowid: i64) -> anyhow::Result<MelodyInfo> {
        let connection = self.get_connection()?;
        let melody = self.melody(&connection, rowid)?;
        Self::info_for(&connection, rowid, melody)
    }

    fn info_for(connection: &Connection, rowid: i64, melody: Melody) -> anyhow::Result<MelodyInfo> {
        let mut statement = connection
            .prepare("SELECT rowid, timestamp, rating FROM melody_index WHERE rowid = ?")?;
        statement.bind((1, rowid))?;
        if let State::Row = statement.next()? {
            let timestamp = statement.read::<i64, usize>(1)?;
            let rating = statement.read::<String, usize>(2)?.parse::<Preference>()?;
            let tags = Self::tags_for(connection, rowid)?;
            Ok(MelodyInfo {
                rowid,
                timestamp,
                rating,
                tags,
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
                .prepare("SELECT pitch, duration, velocity FROM melodies WHERE melody_row = ?")?;
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

    pub fn progression(&mut self, connection: &Connection, rowid: i64) -> anyhow::Result<Vec<Chord>> {
        let mut statement = connection.prepare("SELECT chord_row, start, duration FROM progression_times WHERE progression_row = ?")?;
        statement.bind((1, rowid))?;
        let mut result = vec![];
        while let State::Row = statement.next()? {
            let chord_row = statement.read::<i64, usize>(0)?;
            let start = statement.read::<f64, usize>(1)?;
            let duration = statement.read::<f64, usize>(2)?;
            let mut statement = connection.prepare("SELECT pitch, velocity FROM chord_notes WHERE chord_row = ?")?;
            statement.bind((1, chord_row))?;
            let mut notes_velocities = vec![];
            while let State::Row = statement.next()? {
                let note = statement.read::<i64, usize>(0)? as MidiByte;
                let velocity = statement.read::<i64, usize>(1)? as MidiByte;
                notes_velocities.push((note, velocity));
            }
            result.push(Chord::new(start, duration, notes_velocities));
        }
        Ok(result)
    }

    pub fn stats(
        &self,
        connection: &Connection,
        variation_id: i64,
    ) -> anyhow::Result<VariationStats> {
        let mut statement = connection.prepare("SELECT algorithm_name, random_prob, ornament_prob, min_note_duration, whimsify FROM variation_info WHERE variation_row = ?")?;
        statement.bind((1, variation_id))?;
        if let State::Row = statement.next()? {
            let algorithm_name = statement.read::<String, usize>(0)?;
            let random_prob = statement.read::<f64, usize>(1)?;
            let ornament_prob = statement.read::<f64, usize>(2)?;
            let min_note_duration = statement.read::<f64, usize>(3)?;
            let whimsify = statement.read::<i64, usize>(4)? != 0;
            Ok(VariationStats {
                algorithm_name,
                random_prob,
                ornament_prob,
                min_note_duration,
                whimsify,
            })
        } else {
            bail!("{variation_id} not in database.")
        }
    }

    pub fn update_info(&mut self, rowid: i64, rating: Preference) -> anyhow::Result<()> {
        let connection = self.get_connection()?;
        let mut statement =
            connection.prepare("UPDATE melody_index SET rating = ? WHERE rowid = ?")?;
        statement.bind((1, rating.to_string().as_str()))?;
        statement.bind((2, rowid))?;
        statement.next()?;
        Ok(())
    }

    pub fn add_melody_and_variation(
        &mut self,
        melody: &Melody,
        variation: &Melody,
        stats: &VariationStats,
    ) -> anyhow::Result<(MelodyInfo, MelodyInfo)> {
        let player_info = self.store_melody(melody)?;
        let variation_info = self.add_variation(player_info.row_id(), variation, stats)?;
        Ok((player_info, variation_info))
    }

    fn add_variation(
        &mut self,
        melody_id: i64,
        variation: &Melody,
        stats: &VariationStats,
    ) -> anyhow::Result<MelodyInfo> {
        let variation_info = self.store_melody(variation)?;
        let connection = self.get_connection()?;
        let mut statement = connection
            .prepare("INSERT INTO variation_info (variation_row, original_row, algorithm_name, random_prob, ornament_prob, min_note_duration, whimsify) VALUES (?,?,?,?,?,?,?)")?;
        statement.bind((1, variation_info.rowid))?;
        statement.bind((2, melody_id))?;
        statement.bind((3, stats.algorithm_name.as_str()))?;
        statement.bind((4, stats.random_prob))?;
        statement.bind((5, stats.ornament_prob))?;
        statement.bind((6, stats.min_note_duration))?;
        statement.bind((7, if stats.whimsify { 1 } else { 0 }))?;
        statement.next()?;
        Ok(variation_info)
    }

    fn store_melody(&mut self, melody: &Melody) -> anyhow::Result<MelodyInfo> {
        let timestamp = Utc::now().timestamp();
        let rating = Preference::Neutral;
        let connection = self.get_connection()?;
        let mut statement =
            connection.prepare("INSERT INTO melody_index (timestamp, rating) VALUES (?, ?)")?;
        statement.bind((1, timestamp))?;
        statement.bind((2, rating.to_string().as_str()))?;
        statement.next().unwrap();
        let rowid = Self::get_last_row_id(&connection)?;

        for note in melody.iter() {
            let mut statement = connection.prepare(
                "INSERT INTO melodies (melody_row, pitch, duration, velocity) VALUES (?, ?, ?, ?);",
            )?;
            statement.bind((1, rowid))?;
            statement.bind((2, note.pitch() as i64))?;
            statement.bind((3, note.duration()))?;
            statement.bind((4, note.velocity() as i64))?;
            statement.next().unwrap();
        }

        let info = MelodyInfo {
            rowid,
            timestamp,
            rating,
            tags: BTreeSet::new(),
            melody: melody.clone(),
        };
        self.melody_cache.insert(info.rowid, melody.clone());
        Ok(info)
    }

    pub fn store_chord_progression(&mut self, progression: &Vec<Chord>) -> anyhow::Result<()> {
        let timestamp = Utc::now().timestamp();
        let rating = Preference::Neutral;
        let connection = self.get_connection()?;
        let mut statement = connection
            .prepare("INSERT INTO progression_index (timestamp, rating) VALUES (?, ?)")?;
        statement.bind((1, timestamp))?;
        statement.bind((2, rating.to_string().as_str()))?;
        statement.next().unwrap();
        let progression_row = Self::get_last_row_id(&connection)?;

        for chord in progression.iter() {
            let mut statement = connection.prepare("INSERT INTO chord_index (timestamp) VALUES (?);")?;
            statement.bind((1, timestamp))?;
            statement.next().unwrap();
            let chord_row = Self::get_last_row_id(&connection)?;
            for (note, velocity) in chord.notes_velocities() {
                let mut statement = connection.prepare("INSERT INTO chord_notes (chord_row, pitch, velocity) VALUES (?, ?, ?")?;
                statement.bind((1, chord_row))?;
                statement.bind((2, note as i64))?;
                statement.bind((3, velocity as i64))?;
                statement.next().unwrap();
            }

            let mut statement = connection.prepare(
                "INSERT INTO progression_times (progression_row, chord_row, start, duration) VALUES (?, ?, ?, ?);")?;
            statement.bind((1, progression_row))?;
            statement.bind((2, chord_row))?;
            statement.bind((3, chord.start().0))?;
            statement.bind((4, chord.duration().0))?;
            statement.next().unwrap();
        }
        Ok(())
    }

    fn get_last_row_id(connection: &Connection) -> sqlite::Result<i64> {
        let mut statement = connection.prepare("SELECT last_insert_rowid()")?;
        statement.next()?;
        statement.read::<i64, usize>(0)
    }

}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MelodyInfo {
    rowid: i64,
    timestamp: i64,
    rating: Preference,
    tags: BTreeSet<String>,
    melody: Melody,
}

impl MelodyInfo {
    pub fn row_id(&self) -> i64 {
        self.rowid
    }

    pub fn timestamp(&self) -> i64 {
        self.timestamp
    }

    pub fn tags(&self) -> &BTreeSet<String> {
        &self.tags
    }

    pub fn date(&self) -> NaiveDate {
        Local.timestamp_opt(self.timestamp, 0).unwrap().date_naive()
    }

    pub fn time(&self) -> NaiveTime {
        Local.timestamp_opt(self.timestamp, 0).unwrap().time()
    }

    pub fn date_time_stamp(&self) -> String {
        format!("{:?} {:?}", self.date(), self.time())
    }

    pub fn scale_name(&self) -> String {
        self.melody.best_scale_for().name()
    }

    pub fn rating(&self) -> Preference {
        self.rating
    }

    pub fn set_rating(&mut self, pref: Preference) {
        self.rating = pref;
    }

    pub fn update_preference(&self) -> GuiDatabaseUpdate {
        GuiDatabaseUpdate::Info {
            rowid: self.rowid,
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

impl Preference {
    pub fn sql_choice_str(&self) -> &'static str {
        match self {
            Preference::Favorite => "rating = 'Favorite'",
            Preference::Neutral => "rating <> 'Ignore'",
            Preference::Ignore => "rating LIKE '%'",
        }
    }
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
