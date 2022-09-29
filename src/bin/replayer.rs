use anyhow::bail;
use midir::{MidiInput, Ignore, MidiInputPort};
use musicserver1::input_cmd;
use midi_msg::MidiMsg;

fn main() -> anyhow::Result<()> {
    let mut midi_in = MidiInput::new("midir reading input")?;
    midi_in.ignore(Ignore::None);

    let in_ports = midi_in.ports();
    let in_port = match in_ports.len() {
        0 => bail!("no input port found"),
        1 => {
            println!("Choosing the only available input port: {}", midi_in.port_name(&in_ports[0]).unwrap());
            &in_ports[0]
        },
        _ => {
            println!("\nAvailable input ports:");
            for (i, p) in in_ports.iter().enumerate() {
                println!("{}: {}", i, midi_in.port_name(p).unwrap());
            }
            let input = input_cmd("Please select input port: ")?;
            match in_ports.get(input.trim().parse::<usize>()?) {
                None => bail!("invalid input port selected"),
                Some(p) => p
            }
        }
    };

    println!("\nOpening connection");
    let in_port_name = midi_in.port_name(in_port)?;

    // _conn_in needs to be a named parameter, because it needs to be kept alive until the end of the scope
    let _conn_in = midi_in.connect(in_port, "midir-read-input", move |stamp, message, _| {
        println!("{}: {:?} (len = {})", stamp, message, message.len());
        let (msg, len) = MidiMsg::from_midi(&message).unwrap();
        println!("msg: {:?} len: {}", msg, len);
    }, ())?;

    println!("Connection open, reading input from '{}'", in_port_name);

    let input = input_cmd("(press enter to exit)...")?;
    println!("Closing connection");
    Ok(())
}