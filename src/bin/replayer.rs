use std::sync::{Arc, Mutex};
use midir::{MidiInput, MidiInputPort};
use midi_msg::MidiMsg;
use read_input::prelude::*;
use crossbeam_queue::SegQueue;
use musicserver1::{get_midi_device, make_ai_table, start_ai,  prob_slider, replay_slider, make_synth_table, start_output};

// MIDI input code based on:
//   https://github.com/Boddlnagg/midir/blob/master/examples/test_read_input.rs
// Synthesizer output code based on:
//   https://github.com/SamiPerttu/fundsp/blob/master/examples/beep.rs

// This happens sometimes, not sure why:
//
// thread 'main' panicked at 'called `Result::unwrap()` on an `Err` value: Other("could not create Windows MM MIDI input port")'
// Unplugging and re-plugging seems to do the job.
// I wonder if it was because Sonic Pi was running....

fn main() -> anyhow::Result<()> {
    println!("Welcome to replayer");
    let mut midi_in = MidiInput::new("midir reading input")?;
    let in_port = get_midi_device(&mut midi_in)?;

    let input2ai = Arc::new(SegQueue::new());
    let ai2output = Arc::new(SegQueue::new());

    run_output(ai2output.clone());
    run_ai(input2ai.clone(), ai2output);
    run_input(input2ai, midi_in, in_port)
}


fn run_output(ai2output: Arc<SegQueue<MidiMsg>>) {
    let mut synth_table = make_synth_table();
    synth_table.console_pick();
    start_output(ai2output, Arc::new(Mutex::new(synth_table)));
}

fn run_input(input2ai: Arc<SegQueue<MidiMsg>>, midi_in: MidiInput, in_port: MidiInputPort) -> anyhow::Result<()> {
    println!("\nOpening connection");
    let in_port_name = midi_in.port_name(&in_port)?;

    // _conn_in needs to be a named parameter, because it needs to be kept alive until the end of the scope
    let _conn_in = midi_in.connect(&in_port, "midir-read-input", move |_stamp, message, _| {
        let (msg, _len) = MidiMsg::from_midi(&message).unwrap();
        input2ai.push(msg);
    }, ()).unwrap();

    println!("Connection open, reading input from '{in_port_name}'");

    let _ = input::<String>().msg("(press enter to exit)...\n").get();
    println!("Closing connection");
    Ok(())
}

fn run_ai(input2ai: Arc<SegQueue<MidiMsg>>, ai2output: Arc<SegQueue<MidiMsg>>) {
    let mut ai_table = make_ai_table();
    ai_table.console_pick();
    let mut p_random = prob_slider();
    p_random.console_pick("Select probability of random variation: ");
    let mut replay_delay = replay_slider();
    replay_delay.console_pick("Select time delay before starting replay: ");
    start_ai(Arc::new(Mutex::new(ai_table)), input2ai, ai2output,
             Arc::new(Mutex::new(replay_delay)),
             Arc::new(Mutex::new(p_random)));
}
