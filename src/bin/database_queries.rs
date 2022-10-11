use musicserver1::{Database, MelodyInfo};

fn main() {
    let database = Database::new();
    let main_melodies = MelodyInfo::get_main_melodies(database.clone());
    for info in main_melodies.iter() {
        println!("{info:?}");
        println!("{:?} {:?}", info.get_date(), info.get_time());
        let variations = info.get_variations_of(database.clone());
        for info in variations.iter() {
            println!("{info:?}");
            println!("{:?} {:?}", info.get_date(), info.get_time());
        }
        println!();
    }
}