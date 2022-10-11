use musicserver1::{Database, MelodyInfo};

fn main() -> anyhow::Result<()> {
    let database = Database::new()?;
    let database = database.lock().unwrap();
    for info in database.melodies() {
        print_info(&info);
        for var_info in database.variations_of(info.get_row_id()) {
            print_info(&var_info);
        }
        println!();
    }
    Ok(())
}

fn print_info(info: &MelodyInfo) {
    println!("{info:?}");
    println!("{:?} {:?}", info.get_date(), info.get_time());
}