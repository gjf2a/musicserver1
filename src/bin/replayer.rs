use std::fmt::Debug;
use std::sync::Arc;
use anyhow::bail;
use midir::{MidiInput, Ignore, MidiInputPort};
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use fundsp::hacker::*;
use crossbeam_queue::SegQueue;
use dashmap::DashSet;
use enum_iterator::{all, Sequence};
use read_input::prelude::*;

// MIDI input code based on:
//   https://github.com/Boddlnagg/midir/blob/master/examples/test_read_input.rs
// Synthesizer output code based on:
//   https://github.com/SamiPerttu/fundsp/blob/master/examples/beep.rs

fn main() -> anyhow::Result<()> {
    let mut midi_in = MidiInput::new("midir reading input")?;
    let in_port = get_midi_device(&mut midi_in)?;

    let midi_queue = Arc::new(SegQueue::new());
    start_output(midi_queue.clone());
    start_input(midi_queue, midi_in, in_port)
}

fn user_pick_element<T: Clone, S: Fn(&T) -> String>(choices: impl Iterator<Item=T>, show: S) -> T {
    let choices = choices.collect::<Vec<_>>();
    for (i, item) in choices.iter().enumerate() {
        println!("{}) {}", i+1, show(item));
    }
    let choice: usize = input()
        .msg("Enter choice: ")
        .inside(1..=choices.len())
        .get();
    choices[choice - 1].clone()
}

fn get_midi_device(midi_in: &mut MidiInput) -> anyhow::Result<MidiInputPort> {
    midi_in.ignore(Ignore::None);

    let in_ports = midi_in.ports();
    match in_ports.len() {
        0 => bail!("no input port found"),
        1 => {
            println!("Choosing the only available input port: {}", midi_in.port_name(&in_ports[0]).unwrap());
            Ok(in_ports[0].clone())
        },
        _ => {
            println!("\nAvailable input ports:");
            Ok(user_pick_element(in_ports.iter().cloned(), |p| midi_in.port_name(p).unwrap()))
        }
    }
}

fn start_output(midi_queue: Arc<SegQueue<MidiMsg>>) {
    let host = cpal::default_host();
    let device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config = device.default_output_config().unwrap();

    let synth_funcs = vec![
        SynthFunc {name: "Sine Pulse".to_owned(), func: Arc::new(sine_pulse)},
        SynthFunc {name: "Simple Triangle".to_owned(), func: Arc::new(simple_tri)}
    ];

    let synth_func = pick_synth_func(&synth_funcs);

    match config.sample_format() {
        cpal::SampleFormat::F32 => run::<f32>(midi_queue.clone(), device, config.into(), synth_func).unwrap(),
        cpal::SampleFormat::I16 => run::<i16>(midi_queue.clone(), device, config.into(), synth_func).unwrap(),
        cpal::SampleFormat::U16 => run::<u16>(midi_queue.clone(), device, config.into(), synth_func).unwrap(),
    }
}

fn start_input(midi_queue: Arc<SegQueue<MidiMsg>>, midi_in: MidiInput, in_port: MidiInputPort) -> anyhow::Result<()> {
    println!("\nOpening connection");
    let in_port_name = midi_in.port_name(&in_port)?;

    // _conn_in needs to be a named parameter, because it needs to be kept alive until the end of the scope
    let _conn_in = midi_in.connect(&in_port, "midir-read-input", move |_stamp, message, _| {
        let (msg, _len) = MidiMsg::from_midi(&message).unwrap();
        midi_queue.push(msg);
    }, ()).unwrap();

    println!("Connection open, reading input from '{in_port_name}'");

    let _ = input::<String>().msg("(press enter to exit)...\n").get();
    println!("Closing connection");
    Ok(())
}

#[derive(Clone)]
struct SynthFunc {
    name: String,
    func: Arc<dyn Fn(f64,f64) -> Box<dyn AudioUnit64> + Sync + Send>
}

fn pick_synth_func(funcs: &Vec<SynthFunc>) -> SynthFunc {
    user_pick_element(funcs.iter().cloned(), |sf| sf.name.clone())
}

// If I want to refactor this into function objects at some point, read this first:
// https://stackoverflow.com/a/59442384/906268
#[derive(Copy,Clone,Sequence,Debug)]
enum SynthSound {
    SinPulse, SimpleTri
}

impl SynthSound {
    fn sound(&self, pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
        match self {
            SynthSound::SinPulse => {
                Box::new(lfo(move |t| {
                    (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))
                }) >> pulse() * volume)
            }
            SynthSound::SimpleTri => {
                Box::new(lfo(move |_t| pitch) >> triangle() * volume)
            }
        }
    }

    fn pick_synth() -> Self {
        user_pick_element(all::<Self>(), |s| format!("{:?}", s))
    }
}

fn sine_pulse(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |t| {
        (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))
    }) >> pulse() * volume)
}

fn simple_tri(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |_t| pitch) >> triangle() * volume)
}

fn run<T>(incoming: Arc<SegQueue<MidiMsg>>, device: cpal::Device, config: cpal::StreamConfig, synth: SynthFunc) -> anyhow::Result<()>
    where
        T: cpal::Sample,
{
    let run_inst = RunInstance {
        synth: synth.clone(),
        sample_rate: config.sample_rate.0 as f64,
        channels: config.channels as usize,
        incoming: incoming.clone(),
        device: Arc::new(device),
        config: Arc::new(config),
        notes_in_use: Arc::new(DashSet::new())
    };

    std::thread::spawn(move || {
        run_inst.listen_play_loop::<T>();
    });

    Ok(())
}

#[derive(Clone)]
struct RunInstance {
    synth: SynthFunc,
    sample_rate: f64,
    channels: usize,
    incoming: Arc<SegQueue<MidiMsg>>,
    device: Arc<cpal::Device>,
    config: Arc<cpal::StreamConfig>,
    notes_in_use: Arc<DashSet<u8>>
}

impl RunInstance {
    fn listen_play_loop<T: cpal::Sample>(&self) {
        loop {
            if let Some(m) = self.incoming.pop() {
                if let MidiMsg::ChannelVoice { channel:_, msg} = m {
                    println!("{msg:?}");
                    match msg {
                        ChannelVoiceMsg::NoteOff {note, velocity:_} => {
                            self.notes_in_use.remove(&note);
                        }
                        ChannelVoiceMsg::NoteOn {note, velocity} => {
                            self.notes_in_use.insert(note);
                            let pitch = midi_hz(note as f64);
                            let volume = velocity as f64 / i8::MAX as f64;
                            let mut c = (self.synth.func)(pitch, volume);
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

// Borrowed unchanged from https://github.com/SamiPerttu/fundsp/blob/master/examples/beep.rs
fn write_data<T>(output: &mut [T], channels: usize, next_sample: &mut dyn FnMut() -> (f64, f64))
    where
        T: cpal::Sample,
{
    for frame in output.chunks_mut(channels) {
        let sample = next_sample();
        let left: T = cpal::Sample::from::<f32>(&(sample.0 as f32));
        let right: T = cpal::Sample::from::<f32>(&(sample.1 as f32));

        for (channel, sample) in frame.iter_mut().enumerate() {
            if channel & 1 == 0 {
                *sample = left;
            } else {
                *sample = right;
            }
        }
    }
}