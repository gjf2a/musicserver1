pub mod notes;

// Example Sonic Pi code that communicates with this:
/*
# There is a melody named 'countdown'

require 'socket'

s = TCPSocket.open('localhost', 8888)
s.puts("create_variation 0.5 0.5")
shipment = countdown.join(",")
print shipment
s.puts(shipment)
reply = s.gets
s.close

reply = eval(reply)
print reply
sleep(4)

play_melody countdown, :additive_1
sleep(1)
play_melody reply, :additive_2

 */

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
    println!("{}", melody.best_scale_for().name());
    let cmd_params = command.split_whitespace().collect::<Vec<_>>();
    if cmd_params.len() == 1 && cmd_params[0] == "show_melody" {
        println!("Echoing...");
        write!(stream, "{}", melody.sonic_pi_list())
    } else if cmd_params.len() == 2 && cmd_params[0] == "create_variation_1" {
        let p_rewrite: f64 = cmd_params[1].parse().unwrap();
        let reply = MelodyMaker::new().create_variation_1(&melody, p_rewrite);
        println!("Sending {}", reply.sonic_pi_list());
        write!(stream, "{}", reply.sonic_pi_list())
    } else if cmd_params.len() == 3 && cmd_params[0] == "create_variation_2" {
        let p_rewrite: f64 = cmd_params[1].parse().unwrap();
        let p_3: f64 = cmd_params[2].parse().unwrap();
        let reply = MelodyMaker::new().create_variation_2(&melody, p_rewrite, p_3);
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