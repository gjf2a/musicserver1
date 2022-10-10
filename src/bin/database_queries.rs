use musicserver1::{get_main_melody_ids, get_melody, get_variations_of};
use chrono::{TimeZone, Local};

fn main() {
    let main_melodies = get_main_melody_ids();
    for (row_id, timestamp) in main_melodies.iter() {
        let datetime = Local.timestamp(*timestamp, 0);
        println!("{row_id}: {timestamp} {:?} {:?}", datetime.date(), datetime.time());
        let melody = get_melody(*row_id);
        println!("{:?}", melody);
        let variations = get_variations_of(*row_id);
        for (var_id, var_timestamp) in variations.iter() {
            let datetime = Local.timestamp(*var_timestamp, 0);
            println!("{var_id}: {timestamp} {:?} {:?}", datetime.date(), datetime.time());
            let melody = get_melody(*var_id);
            println!("{:?}", melody);
        }
        println!();
    }
}