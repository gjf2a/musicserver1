use std::collections::BTreeMap;
use std::ops::RangeInclusive;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use crossbeam_queue::SegQueue;
use dashmap::DashSet;
use fundsp::hacker::{lerp11, lfo, midi_hz, pulse, sin_hz, triangle};
use midir::{MidiInput, MidiInputConnection, MidiInputPort};
use read_input::InputBuild;
use read_input::prelude::input;
use crate::{Melody, MelodyMaker, Note, velocity2volume};
use fundsp::prelude::AudioUnit64;
use midi_msg::{ChannelVoiceMsg, MidiMsg};

#[macro_export]
macro_rules! func_vec {
    ($t:ident, $( ($s:expr, $f:expr)),+ ) => {vec![$($t::new($s, Arc::new($f)),)+]}
}

#[macro_export]
macro_rules! make_chooser_table {
    ($tabletype:ident, $functype:ident) => {
        #[derive(Clone)]
        pub struct $tabletype {
            name2choice: BTreeMap<String,$functype>,
            names: Vec<String>,
            choices: Vec<$functype>,
            current_name: String
        }
        impl $tabletype {
            pub fn from(choices: &Vec<$functype>) -> Self {
                let current_name = choices.iter().next().unwrap().name().to_string();
                let mut name2choice = BTreeMap::new();
                for choice in choices.iter() {
                    name2choice.insert(choice.name().to_string(), choice.clone());
                }
                let names: Vec<String> = choices.iter().map(|c| c.name().to_string()).collect();
                $tabletype {name2choice, names, current_name, choices: choices.clone()}
            }

            pub fn choose(&mut self, choice: &str) {
                assert!(self.name2choice.contains_key(choice));
                self.current_name = choice.to_owned();
            }

            pub fn current_name(&self) -> &str {
                self.current_name.as_str()
            }

            pub fn current_func(&self) -> $functype {
                self.name2choice.get(self.current_name.as_str()).unwrap().clone()
            }

            pub fn name_vec(&self) -> Vec<String> {
                self.names.clone()
            }

            pub fn console_pick(&mut self) {
                self.current_name = user_pick_element(self.choices.iter().cloned(),
                    |f| f.name().to_string().clone()).name().to_string();
            }
        }
    }
}

make_chooser_table!{AITable, AIFunc}
make_chooser_table!{SynthTable, SynthFunc}

pub fn make_ai_table() -> AITable {
    let ai_funcs = func_vec![AIFunc,
            ("Bypass", |_,_,_| Melody::new()),
            ("Playback", |_, melody, _| melody.clone()),
            ("Greedy Choice", MelodyMaker::create_variation_1),
            ("Emphasis-Anchored Choice", MelodyMaker::create_variation_2),
            ("Consistent Figure Replacement", MelodyMaker::create_variation_4),
            ("Consistent Anchored Replacement", MelodyMaker::create_variation_3)];
    AITable::from(&ai_funcs)
}

pub fn make_synth_table() -> SynthTable {
    let synth_funcs = func_vec![SynthFunc,
            ("Sine Pulse", sine_pulse),
            ("Simple Triangle", simple_tri)];
    SynthTable::from(&synth_funcs)
}

#[derive(Copy, Clone)]
pub struct SliderValue<T: Copy + Clone> {
    current: T,
    lo: T,
    hi: T
}

impl <T: Copy + Clone + std::str::FromStr + PartialOrd + 'static> SliderValue<T> {
    pub fn new(current: T, min: T, max: T) -> Self {
        SliderValue {current, lo: min, hi: max }
    }

    pub fn make_range(&self) -> RangeInclusive<T> {
        self.lo..=self.hi
    }

    pub fn set_current(&mut self, new_current: T) {
        self.current = new_current;
    }

    pub fn get_current(&self) -> T {
        self.current
    }

    pub fn console_pick(&mut self, prompt: &str) {
        self.current = input().msg(prompt).inside(self.make_range()).get();
    }
}

pub fn replay_slider() -> SliderValue<f64> {
    SliderValue::new(1.5, 1.0, 5.0)
}

pub fn prob_slider() -> SliderValue<f64> {
    SliderValue::new(1.0, 0.0, 1.0)
}

pub fn user_pick_element<T: Clone, S: Fn(&T) -> String>(choices: impl Iterator<Item=T>, show: S) -> T {
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

#[derive(Clone)]
pub struct AIFunc {
    name: String,
    func: Arc<dyn Fn(&mut MelodyMaker,&Melody,f64)->Melody + Send + Sync>
}

impl AIFunc {
    pub fn new(name: &str, func: Arc<dyn Fn(&mut MelodyMaker,&Melody,f64)->Melody + Send + Sync>) -> Self {
        AIFunc {name: name.to_owned(), func}
    }
    pub fn name(&self) -> &str {self.name.as_str()}
    pub fn func(&self) -> Arc<dyn Fn(&mut MelodyMaker,&Melody,f64)->Melody + Send + Sync> {
        self.func.clone()
    }
}

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

pub fn sine_pulse(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |t| {
        (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))
    }) >> pulse() * volume)
}

pub fn simple_tri(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |_t| pitch) >> triangle() * volume)
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
                    println!("{msg:?}");
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

pub fn start_input_thread(input2ai: Arc<SegQueue<MidiMsg>>, midi_in: MidiInput, in_port: MidiInputPort) {
    std::thread::spawn(move || {
        let _conn_in = start_input(input2ai, midi_in, in_port);
        loop {}
    });
}

/// Start the input connection. The returned value needs to remain in scope until we
/// are finished receiving MIDI input.
pub fn start_input(input2ai: Arc<SegQueue<MidiMsg>>, midi_in: MidiInput, in_port: MidiInputPort) -> MidiInputConnection<()> {
    midi_in.connect(&in_port, "midir-read-input", move |_stamp, message, _| {
        let (msg, _len) = MidiMsg::from_midi(&message).unwrap();
        input2ai.push(msg);
    }, ()).unwrap()
}

pub fn start_ai_thread(ai_table: Arc<Mutex<AITable>>, input2ai: Arc<SegQueue<MidiMsg>>, ai2output: Arc<SegQueue<MidiMsg>>,
                       replay_delay_slider: Arc<Mutex<SliderValue<f64>>>,
                       p_random_slider: Arc<Mutex<SliderValue<f64>>>) {
    std::thread::spawn(move || {
        let mut maker = MelodyMaker::new();
        loop {
            let player_melody = record_player(input2ai.clone(), ai2output.clone(), replay_delay_slider.clone());
            perform_variation(&mut maker, p_random_slider.clone(), ai_table.clone(), &player_melody, ai2output.clone());
        }
    });
}

fn record_player(input2ai: Arc<SegQueue<MidiMsg>>, ai2output: Arc<SegQueue<MidiMsg>>, replay_delay_slider: Arc<Mutex<SliderValue<f64>>>) -> Melody {
    let mut waiting: Option<(u8, Instant, u8)> = None;
    let mut player_melody = Melody::new();
    loop {
        if let Some(msg) = input2ai.pop() {
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
            let replay_delay = replay_delay_slider.lock().unwrap();
            if velocity == 0 && elapsed > replay_delay.get_current() {
                player_melody.add(Note::from_midi(note, elapsed, velocity));
                break;
            }
        }
    }
    player_melody
}

fn perform_variation(maker: &mut MelodyMaker, p_random_slider: Arc<Mutex<SliderValue<f64>>>, ai_table: Arc<Mutex<AITable>>, player_melody: &Melody, ai2output: Arc<SegQueue<MidiMsg>>) {
    let variation = {
        let p_random = p_random_slider.lock().unwrap();
        let ai_table = ai_table.lock().unwrap();
        (ai_table.current_func().func)(maker, &player_melody, p_random.get_current())
    };

    for note in variation.iter() {
        let (midi, duration) = note.to_midi();
        ai2output.push(midi);
        std::thread::sleep(Duration::from_secs_f64(duration));
    }
}