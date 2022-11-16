use std::collections::BTreeMap;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{Device, Sample, SampleFormat, StreamConfig};
use crossbeam_queue::SegQueue;
use fundsp::hacker::*;
use fundsp::prelude::AudioUnit64;
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use std::sync::{Arc, Mutex};
use crate::analyzer::velocity2volume;
use crate::runtime::{ChooserTable, SHOW_MIDI_MSG, SynthChoice};
use bare_metal_modulo::*;

const MAX_NOTES: usize = 1;

// Invaluable help with the function type: https://stackoverflow.com/a/59442384/906268
pub type SynthFuncType = dyn Fn(u8, u8) -> Box<dyn AudioUnit64> + Send + Sync;
pub type SynthTable = ChooserTable<Arc<SynthFuncType>>;

pub struct SynthOutputMsg {
    pub synth: SynthChoice,
    pub midi: MidiMsg
}

pub fn convert_midi(note: u8, velocity: u8) -> (f64, f64) {
    (midi_hz(note as f64), (velocity2volume(velocity.into())))
}

pub fn start_output_thread(
    ai2output: Arc<SegQueue<SynthOutputMsg>>,
    _human_synth_table: Arc<Mutex<SynthTable>>, // Bring these back another time.
    _ai_synth_table: Arc<Mutex<SynthTable>>,
) {
    let host = cpal::default_host();
    let device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config = device.default_output_config().unwrap();
    match config.sample_format() {
        SampleFormat::F32 => run_synth::<f32>(
            ai2output,
            device,
            config.into(),
        ),
        SampleFormat::I16 => run_synth::<i16>(
            ai2output,
            device,
            config.into(),
        ),
        SampleFormat::U16 => run_synth::<u16>(
            ai2output,
            device,
            config.into(),
        ),
    }
}

fn run_synth<T: Sample>(
    ai2output: Arc<SegQueue<SynthOutputMsg>>,
    device: Device,
    config: StreamConfig,
) {
    std::thread::spawn(move || {
        let mut stereo: Stereo<MAX_NOTES> = Stereo::new();
        let sample_rate = config.sample_rate.0 as f64;
        let mut sound = stereo.sound();
        sound.reset(Some(sample_rate));
        let mut next_value = move || sound.get_stereo();
        let channels = config.channels as usize;
        let err_fn = |err| eprintln!("Error on stream: {err}");
        let stream = device
            .build_output_stream(
                &config,
                move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
                    write_data(data, channels, &mut next_value)
                },
                err_fn,
            )
            .unwrap();

        stream.play().unwrap();

        loop {
            if let Some(SynthOutputMsg {synth, midi}) = ai2output.pop() {
                if SHOW_MIDI_MSG {
                    println!("synth_output: {midi:?}");
                }
                let side = match synth {
                    SynthChoice::Human => StereoSide::Left,
                    SynthChoice::Ai => StereoSide::Right
                };
                if let MidiMsg::ChannelVoice { channel: _, msg } = midi {
                    match msg {
                        ChannelVoiceMsg::NoteOff { note, velocity: _ } => {
                            stereo.note_off(note, side);
                        }
                        ChannelVoiceMsg::NoteOn { note, velocity } => {
                            stereo.note_on(note, velocity, side);
                        }
                        _ => {}
                    }
                }
            }
        }
    });
}

#[derive(Clone)]
struct Stereo<const N: usize> {
    left: Vars<N>,
    right: Vars<N>
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum StereoSide {Left, Right}

impl <const N: usize> Stereo<N> {
    pub fn new() -> Self {
        Self {left: Vars::new(), right: Vars::new()}
    }

    fn side(&mut self, side: StereoSide) -> &mut Vars<N> {
        match side {StereoSide::Left => &mut self.left, StereoSide::Right => &mut self.right}
    }

    pub fn note_on(&mut self, pitch: u8, velocity: u8, side: StereoSide) {
        self.side(side).on(pitch, velocity)
    }

    pub fn note_off(&mut self, pitch: u8, side: StereoSide) {
        self.side(side).off(pitch)
    }

    pub fn sound(&self) -> Net64 {
        Net64::stack_op(self.left.sound(), self.right.sound())
    }
}

#[derive(Clone)]
struct Vars<const N: usize> {
    pitches: [An<Var<f64>>; N],
    velocities: [An<Var<f64>>; N],
    next: ModNumC<usize, N>,
    pitch2var: BTreeMap<u8,usize>,
    recent_pitches: [Option<u8>; N],
}

impl <const N: usize> Vars<N> {
    pub fn new() -> Self {
        Self {
            pitches: [(); N].map(|_| var(0, 0.0)),
            velocities: [(); N].map(|_| var(1, 0.0)),
            next: ModNumC::new(0),
            pitch2var: BTreeMap::new(),
            recent_pitches: [None; N]
        }
    }

    pub fn sound_at(&self, i: usize) -> Box<dyn AudioUnit64> {
        let pitch = self.pitches[i].clone();
        let velocity = self.velocities[i].clone();
        Box::new(envelope(move |_| midi_hz(pitch.value())) >> triangle() * (envelope(move |_| velocity.value() / 127.0)))
    }

    pub fn sound(&self) -> Net64 {
        let mut sound = Net64::wrap(self.sound_at(0));
        for i in 1..N {
            sound = Net64::bin_op(sound, Net64::wrap(self.sound_at(i)), FrameAdd::new());
        }
        sound
    }

    pub fn on(&mut self, pitch: u8, velocity: u8) {
        self.pitches[self.next.a()].clone().set_value(pitch as f64);
        self.velocities[self.next.a()].clone().set_value(velocity as f64);
        self.pitch2var.insert(pitch, self.next.a());
        self.recent_pitches[self.next.a()] = Some(pitch);
        self.next += 1;
    }

    pub fn off(&mut self, pitch: u8) {
        if let Some(i) = self.pitch2var.remove(&pitch) {
            if self.recent_pitches[i] == Some(pitch) {
                self.recent_pitches[i] = None;
                self.velocities[i].clone().set_value(0.0);
            }
        }
    }
}

// Borrowed from https://github.com/SamiPerttu/fundsp/blob/master/examples/beep.rs
fn write_data<T: Sample>(
    output: &mut [T],
    channels: usize,
    next_sample: &mut dyn FnMut() -> (f64, f64),
) {
    for frame in output.chunks_mut(channels) {
        let sample = next_sample();
        let left: T = Sample::from::<f32>(&(sample.0 as f32));
        let right: T = Sample::from::<f32>(&(sample.1 as f32));

        for (channel, sample) in frame.iter_mut().enumerate() {
            *sample = if channel & 1 == 0 { left } else { right };
        }
    }
}
