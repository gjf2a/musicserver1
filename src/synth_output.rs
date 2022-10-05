use std::collections::vec_deque::VecDeque;
use std::sync::{Arc, Mutex};
use midi_msg::{ChannelVoiceMsg, MidiMsg};
use crossbeam_queue::SegQueue;
use crossbeam_utils::atomic::AtomicCell;
use crate::ChooserTable;
use fundsp::prelude::AudioUnit64;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use crate::adsr::SoundMsg;

// Invaluable help with the function type: https://stackoverflow.com/a/59442384/906268
pub type SynthFuncType = dyn Fn(u8,u8,Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> + Send + Sync;
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
        config: Arc::new(config)
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
    config: Arc<cpal::StreamConfig>
}

impl RunInstance {
    fn listen_play_loop<T: cpal::Sample>(&mut self) {
        let mut sound_thread_messages: VecDeque<Arc<AtomicCell<SoundMsg>>> = VecDeque::new();
        loop {
            if let Some(m) = self.ai2output.pop() {
                if let MidiMsg::ChannelVoice { channel:_, msg} = m {
                    println!("synth receives {msg:?}");
                    match msg {
                        ChannelVoiceMsg::NoteOff {note:_, velocity:_} => {
                            if let Some(m) = sound_thread_messages.back() {
                                m.store(SoundMsg::Release);
                            }
                        }
                        ChannelVoiceMsg::NoteOn {note, velocity} => {
                            loop {
                                match sound_thread_messages.pop_front() {
                                    None => break,
                                    Some(m) => m.store(SoundMsg::Stop)
                                }
                            }
                            let note_m = Arc::new(AtomicCell::new(SoundMsg::Play));
                            sound_thread_messages.push_back(note_m.clone());
                            let mut sound = {
                                let synth_table = self.synth_table.lock().unwrap();
                                (synth_table.current_choice())(note, velocity, note_m.clone())
                            };
                            sound.reset(Some(self.sample_rate));
                            self.play_sound::<T>(sound, note_m);
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    fn play_sound<T: cpal::Sample>(&self, mut sound: Box<dyn AudioUnit64>, note_m: Arc<AtomicCell<SoundMsg>>) {
        let mut next_value = move || sound.get_stereo();
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
                match note_m.load() {
                    SoundMsg::Stop => break,
                    _ => {}
                }
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
