use fundsp::hacker::{lerp11, lfo, midi_hz, pulse, sin_hz, triangle};
use fundsp::prelude::AudioUnit64;
use std::sync::Arc;
use std::time::Instant;
use dashmap::DashMap;
use crate::{func_vec, SynthTable, SynthFunc, adsr_fixed, velocity2volume, NoteStatus};

pub fn make_synth_table() -> SynthTable {
    let synth_funcs = func_vec![SynthFunc,
            ("Sine Pulse", sine_pulse),
            ("ADSR 1", expr1),
            ("ADSR 2", expr2),
            ("Simple Triangle", simple_tri)];
    SynthTable::from(&synth_funcs)
}

pub fn sine_pulse(pitch: u8, volume: u8, notes_in_use: Arc<DashMap<u8,NoteStatus>>) -> Box<dyn AudioUnit64> {
    let volume = if let Some(status) = notes_in_use.get(&pitch) {
        if let NoteStatus::Release(inst) = status.value() {
            notes_in_use.remove(&pitch);
        }
        0.0
    } else {
        velocity2volume(volume.into())
    };
    let pitch = midi_hz(pitch as f64);
    Box::new(lfo(move |t| {
        (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))
    }) >> pulse() * volume)
}

pub fn simple_tri(pitch: u8, volume: u8, notes_in_use: Arc<DashMap<u8,NoteStatus>>) -> Box<dyn AudioUnit64> {
    let volume = if let Some(status) = notes_in_use.get(&pitch) {
        if let NoteStatus::Release(inst) = status.value() {
            notes_in_use.remove(&pitch);
        }
        0.0
    } else {
        velocity2volume(volume.into())
    };
    let pitch = midi_hz(pitch as f64);
    Box::new(lfo(move |_t| pitch) >> triangle() * volume)
}

pub fn expr1(pitch: u8, volume: u8, releasing: Arc<DashMap<u8,NoteStatus>>) -> Box<dyn AudioUnit64> {
    releasing.remove(&pitch);
    let pitch = midi_hz(pitch as f64);
    let volume = velocity2volume(volume.into());
    Box::new(lfo(move |_t| pitch) >> triangle() * adsr_fixed(0.2, 0.2, 0.4, 0.4, 0.2) * volume)
}

pub fn expr2(pitch: u8, volume: u8, releasing: Arc<DashMap<u8,NoteStatus>>) -> Box<dyn AudioUnit64> {
    releasing.remove(&pitch);
    let pitch = midi_hz(pitch as f64);
    let volume = velocity2volume(volume.into());
    Box::new(lfo(move |t| {(pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))}) >> pulse() * adsr_fixed(0.2, 0.2, 0.4, 0.4, 0.2) * volume)
}
