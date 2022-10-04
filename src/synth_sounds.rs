use fundsp::hacker::{lerp11, lfo, pulse, sin_hz, triangle};
use fundsp::prelude::AudioUnit64;
use crate::{func_vec, SynthTable, SynthFunc};
use std::sync::Arc;

pub fn make_synth_table() -> SynthTable {
    let synth_funcs = func_vec![SynthFunc,
            ("Sine Pulse", sine_pulse),
            ("Simple Triangle", simple_tri)];
    SynthTable::from(&synth_funcs)
}

pub fn sine_pulse(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |t| {
        (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))
    }) >> pulse() * volume)
}

pub fn simple_tri(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |_t| pitch) >> triangle() * volume)
}