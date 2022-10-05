use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Mutex};
use std::time::Instant;
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use crossbeam_queue::SegQueue;
use crate::ChooserTable;
use fundsp::prelude::AudioUnit64;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use fundsp::hacker::lerp;

// Invaluable help with the function type: https://stackoverflow.com/a/59442384/906268
pub type SynthFuncType = dyn Fn(u8,u8,Arc<Mutex<HashMap<u8,Adsr>>>) -> Box<dyn AudioUnit64> + Send + Sync;
pub type SynthTable = ChooserTable<Arc<SynthFuncType>>;

pub fn start_output_thread(ai2output: Arc<SegQueue<MidiMsg>>, synth_table: Arc<Mutex<SynthTable>>) {
    let host = cpal::default_host();
    let device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config = device.default_output_config().unwrap();
    match config.sample_format() {
        cpal::SampleFormat::F32 => run_synth::<f32>(ai2output, device, config.into(), synth_table).unwrap(),
        cpal::SampleFormat::I16 => run_synth::<i16>(ai2output, device, config.into(), synth_table).unwrap(),
        cpal::SampleFormat::U16 => run_synth::<u16>(ai2output, device, config.into(), synth_table).unwrap(),
    }
}

fn run_synth<T>(ai2output: Arc<SegQueue<MidiMsg>>, device: cpal::Device, config: cpal::StreamConfig, synth_table: Arc<Mutex<SynthTable>>) -> anyhow::Result<()>
    where
        T: cpal::Sample,
{
    let mut run_inst = RunInstance {
        synth_table: synth_table.clone(),
        sample_rate: config.sample_rate.0 as f64,
        channels: config.channels as usize,
        ai2output: ai2output.clone(),
        device: Arc::new(device),
        config: Arc::new(config),
        notes_in_use: Arc::new(Mutex::new(HashMap::new()))
    };

    std::thread::spawn(move || {
        run_inst.listen_play_loop::<T>();
    });

    Ok(())
}

#[derive(Clone)]
struct RunInstance {
    synth_table: Arc<Mutex<SynthTable>>,
    sample_rate: f64,
    channels: usize,
    ai2output: Arc<SegQueue<MidiMsg>>,
    device: Arc<cpal::Device>,
    config: Arc<cpal::StreamConfig>,
    notes_in_use: Arc<Mutex<HashMap<u8,Adsr>>>
}

#[derive(Copy, Clone)]
pub struct Adsr {
    attack: f64, decay: f64, sustain: f64, release: f64, release_start: Option<Instant>
}

impl Adsr {
    pub fn new(attack: f64, decay: f64, sustain: f64, release: f64) -> Self {
        Adsr {attack, decay, sustain, release, release_start: None}
    }

    pub fn release(&mut self) {
        self.release_start = Some(Instant::now());
    }

    pub fn volume(&self, time: f64) -> Option<f64> {
        match self.release_start {
            None => {
                if time < self.attack {
                    Some(lerp(0.0, 1.0, time / self.attack))
                } else if time - self.attack < self.decay {
                    Some(lerp(1.0, self.sustain, (time - self.attack) / self.decay))
                } else {
                    Some(self.sustain)
                }
            }
            Some(release_start) => {
                let release_time = time - Self::elapsed(release_start);
                if release_time > self.release {
                    None
                } else {
                    Some(lerp(self.sustain, 0.0, release_time / self.release))
                }
            }
        }
    }

    fn elapsed(instant: Instant) -> f64 {instant.elapsed().as_secs_f64()}
}

impl Debug for Adsr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Adsr {{ attack: {} decay: {} sustain: {} release: {} release_start: {:?} }}",
               self.attack, self.decay, self.sustain, self.release,
               self.release_start.map(Self::elapsed)
        )
    }
}

impl RunInstance {
    fn listen_play_loop<T: cpal::Sample>(&mut self) {
        loop {
            if let Some(m) = self.ai2output.pop() {
                if let MidiMsg::ChannelVoice { channel:_, msg} = m {
                    println!("synth receives {msg:?}");
                    match msg {
                        ChannelVoiceMsg::NoteOff {note, velocity:_} => {
                            self.start_release(note);
                        }
                        ChannelVoiceMsg::NoteOn {note, velocity} => {
                            //self.release_all();
                            {
                                let mut notes_in_use = self.notes_in_use.lock().unwrap();
                                notes_in_use.clear();
                                notes_in_use.insert(note, Adsr::new(0.2, 0.2, 0.4, 0.2));
                            }
                            let synth_table = self.synth_table.lock().unwrap();
                            let mut c = (synth_table.current_choice())(note, velocity, self.notes_in_use.clone());
                            c.reset(Some(self.sample_rate));
                            self.play_sound::<T>(note, c);
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    fn start_release(&self, note: u8) {
        let mut notes_in_use = self.notes_in_use.lock().unwrap();
        match notes_in_use.get_mut(&note) {
            None => {}
            Some(status) => {status.release()}
        }
    }
/*
    fn release_all(&self) {
        for note in self.notes_in_use.iter() {
            self.start_release(*(note.key()));
        }
    }

 */

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
            loop {
                let notes_in_use = notes_in_use.lock().unwrap();
                if !notes_in_use.contains_key(&note) {break;}
            }
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
