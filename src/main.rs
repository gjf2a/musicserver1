mod notes;

use std::io::{Write, BufReader, BufRead};
use std::net::{TcpListener, TcpStream};

fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:8888")?;

    // accept connections and process them serially
    for stream in listener.incoming() {
        handle_client(&mut stream?)?;
    }
    Ok(())
}

fn handle_client(stream: &mut TcpStream) -> std::io::Result<()> {
    let mut reader = BufReader::new(stream.try_clone().unwrap());
    let command = get_trimmed_line(&mut reader)?;
    let scale = get_trimmed_line(&mut reader)?;
    let melody = get_trimmed_line(&mut reader)?;
    println!("Read: {} next {} then {} ", command, scale, melody);
    write!(stream, "echo: [{}] [{}] [{}]", command, scale, melody)?;
    Ok(())
}

fn get_trimmed_line(reader: &mut BufReader<TcpStream>) -> std::io::Result<String> {
    let mut incoming = String::new();
    reader.read_line(&mut incoming)?;
    Ok(incoming.trim().to_string())
}