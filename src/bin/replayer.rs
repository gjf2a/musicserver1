use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};
use midir::{MidiInput, MidiInputPort};
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use fundsp::hacker::*;
use dashmap::DashSet;
use read_input::prelude::*;
use crossbeam_queue::SegQueue;
use musicserver1::{Melody, MelodyMaker, Note, velocity2volume, get_midi_device, user_func_pick, wrap_func, user_pick_element, SynthFunc, sine_pulse, simple_tri, AIFunc, write_data, make_ai_table, start_ai, SliderValue, prob_slider, replay_slider};

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
    let host = cpal::default_host();
    let device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config = device.default_output_config().unwrap();
    let synth = user_func_pick!(SynthFunc, ("Sine Pulse", sine_pulse), ("Simple Triangle", simple_tri));

    match config.sample_format() {
        cpal::SampleFormat::F32 => run::<f32>(ai2output, device, config.into(), synth).unwrap(),
        cpal::SampleFormat::I16 => run::<i16>(ai2output, device, config.into(), synth).unwrap(),
        cpal::SampleFormat::U16 => run::<u16>(ai2output, device, config.into(), synth).unwrap(),
    }
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
    p_random.console_pick("Select probability of random variation");
    let mut replay_delay = replay_slider();
    replay_delay.console_pick("Select time delay before starting replay");
    start_ai(Arc::new(Mutex::new(ai_table)), input2ai, ai2output,
             Arc::new(Mutex::new(replay_delay)),
             Arc::new(Mutex::new(p_random)));
}

fn run<T>(ai2output: Arc<SegQueue<MidiMsg>>, device: cpal::Device, config: cpal::StreamConfig, synth: SynthFunc) -> anyhow::Result<()>
    where
        T: cpal::Sample,
{
    let run_inst = RunInstance {
        synth: synth.clone(),
        sample_rate: config.sample_rate.0 as f64,
        channels: config.channels as usize,
        ai2output: ai2output.clone(),
        device: Arc::new(device),
        config: Arc::new(config),
        notes_in_use: Arc::new(DashSet::new())
    };

    thread::spawn(move || {
        run_inst.listen_play_loop::<T>();
    });

    Ok(())
}

#[derive(Clone)]
struct RunInstance {
    synth: SynthFunc,
    sample_rate: f64,
    channels: usize,
    ai2output: Arc<SegQueue<MidiMsg>>,
    device: Arc<cpal::Device>,
    config: Arc<cpal::StreamConfig>,
    notes_in_use: Arc<DashSet<u8>>
}

impl RunInstance {
    fn listen_play_loop<T: cpal::Sample>(&self) {
        loop {
            if let Some(m) = self.ai2output.pop() {
                if let MidiMsg::ChannelVoice { channel:_, msg} = m {
                    println!("{msg:?}");
                    match msg {
                        ChannelVoiceMsg::NoteOff {note, velocity:_} => {
                            self.notes_in_use.remove(&note);
                        }
                        ChannelVoiceMsg::NoteOn {note, velocity} => {
                            self.notes_in_use.insert(note);
                            let pitch = midi_hz(note as f64);
                            let volume = velocity2volume(velocity.into());
                            let mut c = (self.synth.func())(pitch, volume);
                            c.reset(Some(self.sample_rate));
                            println!("{:?}", c.get_stereo());
                            self.play_sound::<T>(note, c);
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    fn play_sound<T: cpal::Sample>(&self, note: u8, mut sound: Box<dyn AudioUnit64>) {
        let mut next_value = move || sound.get_stereo();
        let notes_in_use = self.notes_in_use.clone();
        let device = self.device.clone();
        let config = self.config.clone();
        let channels = self.channels;
        std::thread::spawn(move || {
            let err_fn = |err| eprintln!("an error occurred on stream: {err}");
            let stream = device.build_output_stream(
                &config,
                move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
                    write_data(data, channels, &mut next_value)
                },
                err_fn,
            ).unwrap();

            stream.play().unwrap();
            while notes_in_use.contains(&note) {}
        });
    }
}