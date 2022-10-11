use chrono::Utc;

fn main() {
    let now = Utc::now();
    println!("{now:?}");
    let t = now.timestamp();
    println!("{t}");
}