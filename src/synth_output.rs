use crate::analyzer::velocity2volume;
use crate::runtime::{ChooserTable, SynthChoice, SHOW_MIDI_MSG};
use bare_metal_modulo::*;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{Device, Sample, SampleFormat, Stream, StreamConfig};
use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use fundsp::hacker::*;
use fundsp::prelude::AudioUnit64;
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

const MAX_NOTES: usize = 1;

pub type SynthType = (Box<dyn AudioUnit64>, (f64, f64, f64, f64));
pub type SynthTable = ChooserTable<SynthType>;

pub enum SynthOutputMsg {
    Play { synth: SynthChoice, midi: MidiMsg },
    StopAll,
}

pub fn convert_midi(note: u8, velocity: u8) -> (f64, f64) {
    (midi_hz(note as f64), (velocity2volume(velocity.into())))
}

pub fn start_output_thread(
    ai2output: Arc<SegQueue<SynthOutputMsg>>,
    human_synth_table: Arc<Mutex<SynthTable>>,
    human_synth_id: Arc<AtomicCell<usize>>,
    ai_synth_table: Arc<Mutex<SynthTable>>,
    ai_synth_id: Arc<AtomicCell<usize>>,
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
            human_synth_table,
            human_synth_id,
            ai_synth_table,
            ai_synth_id,
        ),
        SampleFormat::I16 => run_synth::<i16>(
            ai2output,
            device,
            config.into(),
            human_synth_table,
            human_synth_id,
            ai_synth_table,
            ai_synth_id,
        ),
        SampleFormat::U16 => run_synth::<u16>(
            ai2output,
            device,
            config.into(),
            human_synth_table,
            human_synth_id,
            ai_synth_table,
            ai_synth_id,
        ),
    }
}

/// Used a macro because returned tuples can't mutate separate variables.
macro_rules! reset_synth {
    ($current:ident, $new:ident, $table:ident, $id:ident) => {
        $current = $id.load();
        let $new = {
            let $table = $table.lock().unwrap();
            $table.current_choice().clone()
        };
    };
}

fn run_synth<T: Sample>(
    ai2output: Arc<SegQueue<SynthOutputMsg>>,
    device: Device,
    config: StreamConfig,
    human_synth_table: Arc<Mutex<SynthTable>>,
    human_synth_id: Arc<AtomicCell<usize>>,
    ai_synth_table: Arc<Mutex<SynthTable>>,
    ai_synth_id: Arc<AtomicCell<usize>>,
) {
    std::thread::spawn(move || {
        let mut stereo: StereoSounds<MAX_NOTES> = StereoSounds::new();
        let mut left_id;
        let mut right_id;

        loop {
            reset_synth!(left_id, left_synth, human_synth_table, human_synth_id);
            reset_synth!(right_id, right_synth, ai_synth_table, ai_synth_id);
            stereo.change_synths(left_synth, right_synth);
            let stream = get_stream::<T>(&stereo, &config, &device);
            stream.play().unwrap();

            while left_id == human_synth_id.load() && right_id == ai_synth_id.load() {
                if let Some(msg) = ai2output.pop() {
                    match msg {
                        SynthOutputMsg::Play { synth, midi } => {
                            midi2stereo(&mut stereo, synth, midi)
                        }
                        SynthOutputMsg::StopAll => stereo.all_off(),
                    }
                }
            }
        }
    });
}

fn get_stream<T: Sample>(
    stereo: &StereoSounds<MAX_NOTES>,
    config: &StreamConfig,
    device: &Device,
) -> Stream {
    let sample_rate = config.sample_rate.0 as f64;
    let mut sound = stereo.sound();
    sound.reset(Some(sample_rate));
    let mut next_value = move || sound.get_stereo();
    let channels = config.channels as usize;
    let err_fn = |err| eprintln!("Error on stream: {err}");
    device
        .build_output_stream(
            &config,
            move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
                write_data(data, channels, &mut next_value)
            },
            err_fn,
        )
        .unwrap()
}

fn midi2stereo(stereo: &mut StereoSounds<MAX_NOTES>, synth: SynthChoice, midi: MidiMsg) {
    if SHOW_MIDI_MSG {
        println!("synth_output: {midi:?}");
    }
    let side = match synth {
        SynthChoice::Original => StereoSide::Left,
        SynthChoice::Variation => StereoSide::Right,
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

#[derive(Clone)]
struct StereoSounds<const N: usize> {
    left: LiveSounds<N>,
    right: LiveSounds<N>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum StereoSide {
    Left,
    Right,
}

impl<const N: usize> StereoSounds<N> {
    pub fn new() -> Self {
        Self {
            left: LiveSounds::new(),
            right: LiveSounds::new(),
        }
    }

    pub fn change_synths(&mut self, new_left_synth: SynthType, new_right_synth: SynthType) {
        self.left.change_synth(new_left_synth);
        self.right.change_synth(new_right_synth);
    }

    fn side(&mut self, side: StereoSide) -> &mut LiveSounds<N> {
        match side {
            StereoSide::Left => &mut self.left,
            StereoSide::Right => &mut self.right,
        }
    }

    pub fn note_on(&mut self, pitch: u8, velocity: u8, side: StereoSide) {
        self.side(side).on(pitch, velocity)
    }

    pub fn note_off(&mut self, pitch: u8, side: StereoSide) {
        self.side(side).off(pitch)
    }

    pub fn all_off(&mut self) {
        self.left.all_off();
        self.right.all_off();
    }

    pub fn sound(&self) -> Net64 {
        Net64::stack_op(self.left.sound(), self.right.sound())
    }
}

#[derive(Clone)]
struct LiveSounds<const N: usize> {
    pitches: [An<Var<f64>>; N],
    velocities: [An<Var<f64>>; N],
    controls: [An<Var<f64>>; N],
    next: ModNumC<usize, N>,
    pitch2var: BTreeMap<u8, usize>,
    recent_pitches: [Option<u8>; N],
    synth: Box<dyn AudioUnit64>,
    adsr: (f64, f64, f64, f64),
}

impl<const N: usize> LiveSounds<N> {
    pub fn new() -> Self {
        Self {
            pitches: [(); N].map(|_| var(0, 0.0)),
            velocities: [(); N].map(|_| var(1, 0.0)),
            controls: [(); N].map(|_| var(2, 0.0)),
            next: ModNumC::new(0),
            pitch2var: BTreeMap::new(),
            recent_pitches: [None; N],
            synth: Box::new(triangle()),
            adsr: (0.1, 0.1, 0.5, 0.1),
        }
    }

    pub fn change_synth(&mut self, new_synth: SynthType) {
        self.synth = new_synth.0;
        self.adsr = new_synth.1;
    }

    pub fn sound_at(&self, i: usize) -> Box<dyn AudioUnit64> {
        let pitch = Net64::wrap(Box::new(self.pitches[i].clone()));
        let velocity = Net64::wrap(Box::new(self.velocities[i].clone()));
        let control = Net64::wrap(Box::new(self.controls[i].clone()));
        let adsr = Net64::wrap(Box::new(adsr_live(
            self.adsr.0,
            self.adsr.1,
            self.adsr.2,
            self.adsr.3,
        )));
        Box::new(Net64::bin_op(
            Net64::bin_op(
                Net64::pipe_op(pitch, Net64::wrap(self.synth.clone())),
                velocity,
                FrameMul::new(),
            ),
            Net64::pipe_op(control, adsr),
            FrameMul::new(),
        ))
    }

    pub fn sound(&self) -> Net64 {
        let mut sound = Net64::wrap(self.sound_at(0));
        for i in 1..N {
            sound = Net64::bin_op(sound, Net64::wrap(self.sound_at(i)), FrameAdd::new());
        }
        sound
    }

    pub fn on(&mut self, pitch: u8, velocity: u8) {
        self.pitches[self.next.a()].set_value(midi_hz(pitch as f64));
        self.velocities[self.next.a()].set_value(velocity as f64 / 127.0);
        self.controls[self.next.a()].set_value(1.0);
        self.pitch2var.insert(pitch, self.next.a());
        self.recent_pitches[self.next.a()] = Some(pitch);
        self.next += 1;
    }

    pub fn off(&mut self, pitch: u8) {
        if let Some(i) = self.pitch2var.remove(&pitch) {
            if self.recent_pitches[i] == Some(pitch) {
                self.release(i);
            }
        }
    }

    fn release(&mut self, i: usize) {
        self.recent_pitches[i] = None;
        self.controls[i].set_value(-1.0);
    }

    pub fn all_off(&mut self) {
        for i in 0..N {
            self.release(i);
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
