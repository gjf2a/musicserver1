use musicserver1::database::{Database, MelodyInfo};

fn main() {
    let mut database = Database::new();
    for (info1, info2) in database.get_melody_pairs().unwrap() {
        print_info(&info1);
        print_info(&info2);
        println!();
    }
}

fn print_info(info: &MelodyInfo) {
    println!("{info:?}");
    println!("{:?} {:?}", info.get_date(), info.get_time());
}
