use std::sync::Arc;
use anyhow::bail;
use fundsp::hacker::{lerp11, lfo, pulse, sin_hz, triangle};
use midir::{Ignore, MidiInput, MidiInputPort};
use read_input::InputBuild;
use read_input::prelude::input;
use crate::{Melody, MelodyMaker};
use fundsp::prelude::AudioUnit64;

#[macro_export]
macro_rules! wrap_func {
    ($t:ident, $s:expr, $f:expr) => {
        $t::new($s, Arc::new($f))
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

#[macro_export]
macro_rules! func_map {
    ($t:ident, $( ($s:expr, $f:expr)),+ ) => {
        {
            let mut map = HashMap::new();
            $(
            map.insert($s.to_owned(), wrap_func!($t, $s, $f));
            )+
            map
        }
    }
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

pub fn get_midi_device(midi_in: &mut MidiInput) -> anyhow::Result<MidiInputPort> {
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
}

pub fn sine_pulse(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |t| {
        (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))
    }) >> pulse() * volume)
}

pub fn simple_tri(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |_t| pitch) >> triangle() * volume)
}
