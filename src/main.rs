mod notes;

use std::io::{Write, BufReader, BufRead};
use std::net::{TcpListener, TcpStream};
use crate::notes::{Melody, MelodyMaker};

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
    let melody = get_trimmed_line(&mut reader)?;
    println!("Read: {} then {} ", command, melody);
    let melody = Melody::from(melody.as_str());
    println!("{}", melody.view_notes());
    let cmd_params = command.split_whitespace().collect::<Vec<_>>();
    if cmd_params.len() == 3 && cmd_params[0] == "create_variation" {
        let p_rewrite: f64 = cmd_params[1].parse().unwrap();
        let p_3: f64 = cmd_params[2].parse().unwrap();
        let reply = MelodyMaker::new().create_variation(&melody, p_rewrite, p_3);
        println!("Sending {}", reply.sonic_pi_list());
        write!(stream, "{}", reply.sonic_pi_list())
    } else {
        write!(stream, "Could not process command")
    }
}

fn get_trimmed_line(reader: &mut BufReader<TcpStream>) -> std::io::Result<String> {
    let mut incoming = String::new();
    reader.read_line(&mut incoming)?;
    Ok(incoming.trim().to_string())
}