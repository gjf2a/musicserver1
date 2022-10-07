use std::sync::Arc;
use crossbeam_queue::SegQueue;
use midi_msg::MidiMsg;
use midir::{MidiInput, MidiInputConnection, MidiInputPort};

use crate::SHOW_MIDI_MSG;

pub fn start_input_thread(input2ai: Arc<SegQueue<MidiMsg>>, midi_in: MidiInput, in_port: MidiInputPort) {
    std::thread::spawn(move || {
        let _conn_in = start_input(input2ai, midi_in, in_port);
        loop {}
    });
}

/// Start the input connection. The returned value needs to remain in scope until we
/// are finished receiving MIDI input.
pub fn start_input(input2ai: Arc<SegQueue<MidiMsg>>, midi_in: MidiInput, in_port: MidiInputPort) -> MidiInputConnection<()> {
    midi_in.connect(&in_port, "midir-read-input", move |_stamp, message, _| {
        let (msg, _len) = MidiMsg::from_midi(&message).unwrap();
        if SHOW_MIDI_MSG {
            println!("midi_input: {msg:?}");
        }
        input2ai.push(msg);
    }, ()).unwrap()
}
