use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use crossbeam_queue::SegQueue;
use crate::{velocity2volume, user_pick_element, make_chooser_table};
use fundsp::prelude::{midi_hz, AudioUnit64};
use dashmap::DashSet;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

make_chooser_table!{SynthTable, SynthFunc}

// Invaluable help with the function type: https://stackoverflow.com/a/59442384/906268
#[derive(Clone)]
pub struct SynthFunc {
    name: String,
    func: Arc<dyn Fn(f64,f64) -> Box<dyn AudioUnit64> + Send + Sync>
}

impl SynthFunc {
    pub fn new(name: &str, func: Arc<dyn Fn(f64,f64) -> Box<dyn AudioUnit64> + Send + Sync>) -> Self {
        SynthFunc {name: name.to_owned(), func}
    }

    pub fn name(&self) -> &str {self.name.as_str()}

    pub fn func(&self) -> Arc<dyn Fn(f64,f64) -> Box<dyn AudioUnit64> + Send + Sync> {
        self.func.clone()
    }
}

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
    let run_inst = RunInstance {
        synth_table: synth_table.clone(),
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
    synth_table: Arc<Mutex<SynthTable>>,
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
                    println!("synth receives {msg:?}");
                    match msg {
                        ChannelVoiceMsg::NoteOff {note, velocity:_} => {
                            self.notes_in_use.remove(&note);
                        }
                        ChannelVoiceMsg::NoteOn {note, velocity} => {
                            self.notes_in_use.insert(note);
                            let pitch = midi_hz(note as f64);
                            let volume = velocity2volume(velocity.into());
                            let synth_table = self.synth_table.lock().unwrap();
                            let mut c = (synth_table.current_func().func)(pitch, volume);
                            c.reset(Some(self.sample_rate));
                            println!("stereo values {:?}", c.get_stereo());
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
