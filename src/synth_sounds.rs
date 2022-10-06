use fundsp::hacker::{lerp11, envelope, pulse, sin_hz, triangle};
use fundsp::prelude::AudioUnit64;
use std::sync::Arc;
use crate::{adsr_live, SoundMsg, ChooserTable, SynthFuncType, SynthTable, convert_midi, arc_vec, adsr_fixed};
use crossbeam_utils::atomic::AtomicCell;

pub fn make_synth_table() -> SynthTable {
    let synth_funcs: Vec<(&str, Arc<SynthFuncType>)> = arc_vec![
        ("Live ADSR Pulse", adsr_live_pulse),
        ("Live ADSR Triangle", adsr_live_tri),
        ("Fixed ADSR Pulse", adsr_fixed_pulse),
        ("Fixed ADSR Triangle", adsr_fixed_tri)
    ];
    ChooserTable::from(&synth_funcs)
}

pub fn adsr_live_tri(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(envelope(move |_t| pitch) >> triangle() * adsr_live(0.2, 0.2, 0.4, 0.2, note_m) * volume)
}

pub fn adsr_live_pulse(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(envelope(move |t| (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))) >> pulse() * adsr_live(0.2, 0.2, 0.4, 0.2, note_m) * volume)
}

pub fn adsr_fixed_tri(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(envelope(move |_t| pitch) >> triangle() * adsr_fixed(0.2, 0.2, 0.4, 0.4, 0.2, note_m) * volume)
}

pub fn adsr_fixed_pulse(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(envelope(move |t| (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))) >> pulse() * adsr_fixed(0.2, 0.2, 0.4, 0.4, 0.2, note_m) * volume)
}