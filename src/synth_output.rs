use std::collections::BTreeMap;
use crate::adsr::SoundMsg;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{Device, Sample, SampleFormat, StreamConfig};
use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use fundsp::hacker::midi_hz;
use fundsp::prelude::AudioUnit64;
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use std::collections::vec_deque::VecDeque;
use std::sync::{Arc, Mutex};
use crate::analyzer::velocity2volume;
use crate::runtime::{ChooserTable, SHOW_MIDI_MSG, SynthChoice};

const MAX_SOUNDS: usize = 2;

// Invaluable help with the function type: https://stackoverflow.com/a/59442384/906268
pub type SynthFuncType =
    dyn Fn(u8, u8, Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> + Send + Sync;
pub type SynthTable = ChooserTable<Arc<SynthFuncType>>;

pub struct SynthOutputMsg {
    pub synth: SynthChoice,
    pub midi: MidiMsg,
    pub tag: usize
}

pub fn convert_midi(note: u8, velocity: u8) -> (f64, f64) {
    (midi_hz(note as f64), (velocity2volume(velocity.into())))
}

pub fn start_output_thread(
    ai2output: Arc<SegQueue<SynthOutputMsg>>,
    human_synth_table: Arc<Mutex<SynthTable>>,
    ai_synth_table: Arc<Mutex<SynthTable>>,
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
            ai_synth_table,
        ),
        SampleFormat::I16 => run_synth::<i16>(
            ai2output,
            device,
            config.into(),
            human_synth_table,
            ai_synth_table,
        ),
        SampleFormat::U16 => run_synth::<u16>(
            ai2output,
            device,
            config.into(),
            human_synth_table,
            ai_synth_table,
        ),
    }
}

fn run_synth<T: Sample>(
    ai2output: Arc<SegQueue<SynthOutputMsg>>,
    device: Device,
    config: StreamConfig,
    human_synth_table: Arc<Mutex<SynthTable>>,
    ai_synth_table: Arc<Mutex<SynthTable>>,
) {
    let mut run_inst = RunInstance {
        human_synth_table: human_synth_table.clone(),
        ai_synth_table: ai_synth_table.clone(),
        sample_rate: config.sample_rate.0 as f64,
        channels: config.channels as usize,
        ai2output: ai2output.clone(),
        device: Arc::new(device),
        config: Arc::new(config),
        note2msg: BTreeMap::new(),
        recent_messages: VecDeque::new()
    };

    std::thread::spawn(move || {
        run_inst.listen_play_loop::<T>();
    });
}

#[derive(Clone)]
struct RunInstance {
    human_synth_table: Arc<Mutex<SynthTable>>,
    ai_synth_table: Arc<Mutex<SynthTable>>,
    sample_rate: f64,
    channels: usize,
    ai2output: Arc<SegQueue<SynthOutputMsg>>,
    device: Arc<Device>,
    config: Arc<StreamConfig>,
    note2msg: BTreeMap<(usize,u8),Arc<AtomicCell<SoundMsg>>>,
    recent_messages: VecDeque<Arc<AtomicCell<SoundMsg>>>,
}

impl RunInstance {
    fn listen_play_loop<T: Sample>(&mut self) {
        loop {
            if let Some(SynthOutputMsg {synth, midi, tag}) = self.ai2output.pop() {
                if SHOW_MIDI_MSG {
                    println!("synth_output: {midi:?}");
                }
                if let MidiMsg::ChannelVoice { channel: _, msg } = midi {
                    match msg {
                        ChannelVoiceMsg::NoteOff { note, velocity: _ } => {
                            self.note_off(note, tag);
                        }
                        ChannelVoiceMsg::NoteOn { note, velocity } => {
                            self.note_on::<T>(note, velocity, synth, tag);
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    fn note_on<T: Sample>(&mut self, note: u8, velocity: u8, choice: SynthChoice, tag: usize) {
        self.stop_excessive_notes();
        let note_m = Arc::new(AtomicCell::new(SoundMsg::Play));
        self.note2msg.insert((tag, note), note_m.clone());
        self.recent_messages.push_back(note_m.clone());
        let mut sound = {
            let synth_table = match choice {
                SynthChoice::Human => self.human_synth_table.lock().unwrap(),
                SynthChoice::Ai => self.ai_synth_table.lock().unwrap(),
            };
            (synth_table.current_choice())(note, velocity, note_m.clone())
        };
        sound.reset(Some(self.sample_rate));
        self.play_sound::<T>(sound, note_m);
        println!("Playing {note}");
    }

    fn stop_excessive_notes(&mut self) {
        while self.recent_messages.len() >= MAX_SOUNDS {
            if let Some(m) = self.recent_messages.pop_front() {
                m.store(SoundMsg::Finished);
            }
        }
    }

    fn note_off(&mut self, note: u8, tag: usize) {
        println!("note_off: {note}");
        if let Some(m) = self.note2msg.remove(&(tag, note)) {
            m.store(SoundMsg::Release);
            println!("released");
        }
    }

    fn play_sound<T: Sample>(
        &self,
        mut sound: Box<dyn AudioUnit64>,
        note_m: Arc<AtomicCell<SoundMsg>>
    ) {
        let mut next_value = move || sound.get_stereo();
        let device = self.device.clone();
        let config = self.config.clone();
        let channels = self.channels;
        std::thread::spawn(move || {
            let err_fn = |err| eprintln!("an error occurred on stream: {err}");
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
            Self::loop_until_stop(note_m);
        });
    }

    fn loop_until_stop(note_m: Arc<AtomicCell<SoundMsg>>) {
        loop {
            match note_m.load() {
                SoundMsg::Finished => break,
                _ => {}
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
