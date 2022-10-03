use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};
use anyhow::bail;
use midir::{MidiInput, Ignore, MidiInputPort};
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use fundsp::hacker::*;
use dashmap::DashSet;
use read_input::prelude::*;
use crossbeam_queue::SegQueue;
use musicserver1::{Melody, MelodyMaker, Note, velocity2volume};

#[macro_export]
macro_rules! wrap_func {
    ($t:ident, $s:expr, $f:expr) => {
        $t {name: $s.to_owned(), func: Arc::new($f)}
    }
}

#[macro_export]
macro_rules! user_func_pick {
    ($t:ident, $( ($s:expr, $f:expr)),+ ) => {
        {
        let choices = vec![
            $(
            wrap_func!($t, $s, $f),
            )*
        ];
        user_pick_element(choices.iter().cloned(), |aif| aif.name.clone())
        }
    }
}

// MIDI input code based on:
//   https://github.com/Boddlnagg/midir/blob/master/examples/test_read_input.rs
// Synthesizer output code based on:
//   https://github.com/SamiPerttu/fundsp/blob/master/examples/beep.rs

// This happens sometimes, not sure why:
//
// thread 'main' panicked at 'called `Result::unwrap()` on an `Err` value: Other("could not create Windows MM MIDI input port")'
// Unplugging and re-plugging seems to do the job.

fn main() -> anyhow::Result<()> {
    println!("Welcome to replayer");
    let mut midi_in = MidiInput::new("midir reading input")?;
    let in_port = get_midi_device(&mut midi_in)?;

    let input2ai = Arc::new(SegQueue::new());
    let ai2output = Arc::new(SegQueue::new());

    run_output(ai2output.clone());
    run_ai(input2ai.clone(), ai2output, 1.5, 1.0);
    run_input(input2ai, midi_in, in_port)
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

#[derive(Clone)]
struct AIFunc {
    name: String,
    func: Arc<dyn Fn(&mut MelodyMaker,&Melody,f64)->Melody + Send + Sync>
}

fn run_ai(input2ai: Arc<SegQueue<MidiMsg>>, ai2output: Arc<SegQueue<MidiMsg>>, replay_delay: f64, p_random: f64) {
    let ai_func = user_func_pick!(AIFunc,
        ("Bypass", |_,_,_| Melody::new()),
        ("Playback", |_, melody, _| melody.clone()),
        ("Greedy Choice", MelodyMaker::create_variation_1),
        ("Emphasis-Anchored Choice", MelodyMaker::create_variation_2),
        ("Consistent Figure Replacement", MelodyMaker::create_variation_4),
        ("Consistent Anchored Replacement", MelodyMaker::create_variation_3));
    let mut maker = MelodyMaker::new();

    std::thread::spawn(move || {
        loop {
            let mut waiting: Option<(u8, Instant, u8)> = None;
            let mut player_melody = Melody::new();
            loop {
                if let Some(msg) = input2ai.pop() {
                    println!("AI received {msg:?}");
                    if let MidiMsg::ChannelVoice { channel: _, msg } = msg {
                        match msg {
                            ChannelVoiceMsg::NoteOff { note, velocity } | ChannelVoiceMsg::NoteOn { note, velocity } => {
                                if let Some((w_note, w_timestamp, w_velocity)) = waiting {
                                    player_melody.add(Note::from_midi(w_note, w_timestamp.elapsed().as_secs_f64(), w_velocity));
                                }
                                waiting = Some((note, Instant::now(), velocity));
                            }
                            _ => {}
                        }
                    }
                    ai2output.push(msg);
                }

                if let Some((note, timestamp, velocity)) = waiting {
                    let elapsed = timestamp.elapsed().as_secs_f64();
                    if velocity == 0 && elapsed > replay_delay {
                        player_melody.add(Note::from_midi(note, elapsed, velocity));
                        break;
                    }
                }
            }

            let variation = (ai_func.func)(&mut maker, &player_melody, p_random);
            for note in variation.iter() {
                let (midi, duration) = note.to_midi();
                ai2output.push(midi);
                thread::sleep(Duration::from_secs_f64(duration));
            }
        }
    });
}

// Invaluable help with the function type: https://stackoverflow.com/a/59442384/906268
#[derive(Clone)]
struct SynthFunc {
    name: String,
    func: Arc<dyn Fn(f64,f64) -> Box<dyn AudioUnit64> + Send + Sync>
}

fn sine_pulse(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |t| {
        (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))
    }) >> pulse() * volume)
}

fn simple_tri(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |_t| pitch) >> triangle() * volume)
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