use anyhow::bail;
use crossbeam_queue::SegQueue;
use midi_msg::MidiMsg;
use midir::{Ignore, MidiInput, MidiInputPort};
use musicserver1::{make_ai_table, make_synth_table, prob_slider, replay_slider, start_ai_thread, start_input, start_output_thread, user_pick_element, SynthChoice, ornament_gap_slider};
use read_input::prelude::*;
use std::sync::{Arc, Mutex};

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

fn run_output(ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>) {
    println!("Select synth sound for human");
    let mut human_synth_table = make_synth_table();
    human_synth_table.console_pick();
    println!("Select synth sound for playback");
    let mut ai_synth_table = make_synth_table();
    ai_synth_table.console_pick();
    start_output_thread(
        ai2output,
        Arc::new(Mutex::new(human_synth_table)),
        Arc::new(Mutex::new(ai_synth_table)),
    );
}

fn run_input(
    input2ai: Arc<SegQueue<MidiMsg>>,
    midi_in: MidiInput,
    in_port: MidiInputPort,
) -> anyhow::Result<()> {
    println!("\nOpening connection");
    let in_port_name = midi_in.port_name(&in_port)?;
    let _conn_in = start_input(input2ai, midi_in, in_port);
    println!("Connection open, reading input from '{in_port_name}'");

    let _ = input::<String>().msg("(press enter to exit)...\n").get();
    println!("Closing connection");
    Ok(())
}

fn run_ai(input2ai: Arc<SegQueue<MidiMsg>>, ai2output: Arc<SegQueue<(SynthChoice, MidiMsg)>>) {
    let mut ai_table = make_ai_table();
    ai_table.console_pick();
    let mut p_random = prob_slider();
    p_random.console_pick("Select probability of random variation: ");
    let mut ornament_gap = ornament_gap_slider();
    ornament_gap.console_pick("Select number of notes between ornaments: ");
    let mut replay_delay = replay_slider();
    replay_delay.console_pick("Select time delay before starting replay: ");
    start_ai_thread(
        Arc::new(Mutex::new(ai_table)),
        input2ai,
        ai2output,
        Arc::new(SegQueue::new()),
        Arc::new(Mutex::new(replay_delay)),
        Arc::new(Mutex::new(ornament_gap)),
        Arc::new(Mutex::new(p_random))
    );
}

fn get_midi_device(midi_in: &mut MidiInput) -> anyhow::Result<MidiInputPort> {
    midi_in.ignore(Ignore::None);

    let in_ports = midi_in.ports();
    match in_ports.len() {
        0 => bail!("no input port found"),
        1 => {
            println!(
                "Choosing the only available input port: {}",
                midi_in.port_name(&in_ports[0]).unwrap()
            );
            Ok(in_ports[0].clone())
        }
        _ => {
            println!("\nAvailable input ports:");
            Ok(user_pick_element(in_ports.iter().cloned(), |p| {
                midi_in.port_name(p).unwrap()
            }))
        }
    }
}
