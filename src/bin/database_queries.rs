use musicserver1::MelodyInfo;

fn main() {
    let main_melodies = MelodyInfo::get_main_melodies();
    for info in main_melodies.iter() {
        println!("{info:?}");
        println!("{:?} {:?}", info.get_date(), info.get_time());
        let variations = info.get_variations_of();
        for info in variations.iter() {
            println!("{info:?}");
            println!("{:?} {:?}", info.get_date(), info.get_time());
        }
        println!();
    }
}