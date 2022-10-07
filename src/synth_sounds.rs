use fundsp::hacker::{lerp11, envelope, pulse, sin_hz, triangle, Unop, FrameMulScalar, Pipe, WaveSynth};
use fundsp::prelude::{AudioUnit64, PulseWave};
use std::sync::Arc;
use crate::{adsr_live, SoundMsg, ChooserTable, SynthFuncType, SynthTable, convert_midi, arc_vec, adsr_fixed};
use crossbeam_utils::atomic::AtomicCell;
use fundsp::combinator::An;
use fundsp::envelope::Envelope;
use typenum::{UTerm, UInt, B1};

pub fn make_synth_table() -> SynthTable {
    let synth_funcs: Vec<(&str, Arc<SynthFuncType>)> = arc_vec![
        ("Pulse 1", pulse_1),
        ("Triangle 1", triangle_1),
        ("Pulse 2", pulse_2),
        ("Triangle 2", triangle_2)
    ];
    ChooserTable::from(&synth_funcs)
}

pub fn triangle_1(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(env_triangle(pitch) * adsr1(note_m) * volume)
}

pub fn pulse_1(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(env_pulse_sin(pitch) * adsr1(note_m) * volume)
}

fn env_triangle(pitch: f64) -> An<Pipe<f64, Envelope<f64, f64, impl Fn(f64) -> f64 + Sized, f64>, WaveSynth<'static, f64, UInt<UTerm, B1>>>> {
    envelope(move |_t| pitch) >> triangle()
}

fn env_pulse_sin(pitch: f64) -> An<Pipe<f64, Envelope<f64, f64, impl Fn(f64) -> (f64, f64) + Sized, (f64, f64)>, PulseWave<f64>>> {
    envelope(move |t| (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))) >> pulse()
}

fn adsr1(note_m: Arc<AtomicCell<SoundMsg>>) -> An<Envelope<f64, f64, impl Fn(f64) -> f64 + Sized, f64>> {
    adsr_live(0.2, 0.2, 0.4, 0.2, note_m)
}

pub fn triangle_2(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(env_triangle(pitch) * adsr2(note_m) * volume)
}

pub fn pulse_2(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(env_pulse_sin(pitch) * adsr2(note_m) * volume)
}

fn adsr2(note_m: Arc<AtomicCell<SoundMsg>>) -> An<Unop<f64, Envelope<f64, f64, impl Fn(f64) -> f64 + Sized, f64>, FrameMulScalar<UInt<UTerm, B1>, f64>>> {
    2.0 * adsr_live(0.05, 0.8, 0.2, 0.0, note_m)
}