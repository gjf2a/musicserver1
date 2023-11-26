use midir::{Ignore, InitError, MidiInput, MidiInputPort, MidiInputPorts};

#[derive(Clone)]
pub enum MidiScenario {
    StartingUp,
    NoInputPorts(String),
    InputPortSelected { in_port: MidiInputPort },
    MultipleInputPorts { in_ports: MidiInputPorts },
}

impl MidiScenario {
    pub fn new(midi_in: &mut Result<MidiInput, InitError>) -> Self {
        match midi_in {
            Ok(ref mut midi_in) => {
                midi_in.ignore(Ignore::None);
                let in_ports = midi_in.ports();
                match in_ports.len() {
                    0 => MidiScenario::NoInputPorts(
                        "No MIDI devices found\nRestart program after MIDI device plugged in"
                            .to_string(),
                    ),
                    1 => MidiScenario::InputPortSelected {
                        in_port: in_ports[0].clone(),
                    },
                    _ => MidiScenario::MultipleInputPorts {
                        in_ports: in_ports.clone(),
                    },
                }
            }
            Err(e) => MidiScenario::NoInputPorts(e.to_string()),
        }
    }
}
