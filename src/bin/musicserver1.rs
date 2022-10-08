// Example Sonic Pi code that communicates with this:
/*
# There is a melody named 'countdown'

require 'socket'

s = TCPSocket.open('localhost', 8888)
s.puts("create_variation_1 1.0")
shipment = countdown.join(",")
print shipment
s.puts(shipment)
reply = s.gets
s.close

reply = eval(reply)
print reply
sleep(1)

play_melody countdown, :additive_1
sleep(1)
play_melody reply, :additive_2

 */

use musicserver1::{Melody, MelodyMaker};
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};

fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:8888")?;
    let mut maker = MelodyMaker::new();

    // accept connections and process them serially
    for stream in listener.incoming() {
        handle_client(&mut stream?, &mut maker)?;
    }
    Ok(())
}

fn handle_client(stream: &mut TcpStream, maker: &mut MelodyMaker) -> std::io::Result<()> {
    let mut reader = BufReader::new(stream.try_clone().unwrap());
    let command = get_trimmed_line(&mut reader)?;
    let melody = get_trimmed_line(&mut reader)?;
    println!("Command: {}", command);
    println!("{}", melody);
    let melody = Melody::from(melody.as_str());
    println!("{}", melody.best_scale_for().name());
    println!("{:?}", maker.all_figure_matches(&(melody)));
    println!("{}", melody.view_notes());
    let cmd_params = command.split_whitespace().collect::<Vec<_>>();
    if cmd_params.len() == 1 && cmd_params[0] == "show_melody" {
        println!("Echoing...");
        write!(stream, "{}", melody.sonic_pi_list())
    } else if cmd_params.len() == 2 && cmd_params[0] == "create_variation_1" {
        invoke_variation_func(stream, maker, &melody, &cmd_params, |mk, m, p| {
            mk.create_variation_1(m, p)
        })
    } else if cmd_params.len() == 2 && cmd_params[0] == "create_variation_2" {
        invoke_variation_func(stream, maker, &melody, &cmd_params, |mk, m, p| {
            mk.create_variation_2(m, p)
        })
    } else if cmd_params.len() == 2 && cmd_params[0] == "create_variation_3" {
        invoke_variation_func(stream, maker, &melody, &cmd_params, |mk, m, p| {
            mk.create_variation_3(m, p)
        })?;
        maker.print_figure_mappings();
        Ok(())
    } else if cmd_params.len() == 2 && cmd_params[0] == "create_variation_4" {
        invoke_variation_func(stream, maker, &melody, &cmd_params, |mk, m, p| {
            mk.create_variation_4(m, p)
        })?;
        maker.print_figure_mappings();
        Ok(())
    } else {
        println!("Mystery command: {}", command);
        write!(stream, "Could not process command")
    }
}

fn invoke_variation_func<V: Fn(&mut MelodyMaker, &Melody, f64) -> Melody>(
    stream: &mut TcpStream,
    maker: &mut MelodyMaker,
    melody: &Melody,
    cmd_params: &Vec<&str>,
    make_variation: V,
) -> std::io::Result<()> {
    let p: f64 = cmd_params[1].parse().unwrap();
    let reply = make_variation(maker, melody, p);
    println!("Sending {}", reply.sonic_pi_list());
    println!();
    write!(stream, "{}", reply.sonic_pi_list())
}

fn get_trimmed_line(reader: &mut BufReader<TcpStream>) -> std::io::Result<String> {
    let mut incoming = String::new();
    reader.read_line(&mut incoming)?;
    Ok(incoming.trim().to_string())
}
