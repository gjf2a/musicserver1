use crossbeam_utils::atomic::AtomicCell;
use fundsp::combinator::An;
use fundsp::envelope::Envelope;
use fundsp::hacker::{envelope, lerp11, pulse, sin_hz, triangle, FrameMulScalar, Pipe, Unop, WaveSynth, moog_q, xerp11, constant, Constant};
use fundsp::prelude::{AudioUnit64, PulseWave};
use std::sync::Arc;
use typenum::{UInt, UTerm};
use crate::adsr::{adsr_live, SoundMsg};
use crate::arc_vec;
use crate::runtime::ChooserTable;
use crate::synth_output::{convert_midi, SynthFuncType, SynthTable};

// Moog envelope adapted from fundsp/examples/beep.rs
// I'm using a macro because I could not figure out how to write down the type for $an, which is
// an An<AudioNode> of some sort.
#[macro_export]
macro_rules! moogify {
    ($an:expr) => (($an | envelope(|t| xerp11(110.0, 11000.0, sin_hz(0.60, t)))) >> moog_q(0.6))
}

pub fn make_synth_table() -> SynthTable {
    let synth_funcs: Vec<(&str, Arc<SynthFuncType>)> = arc_vec![
        ("Triangle", triangle_synth),
        ("Triangle Moog", triangle_moog),
        ("Pulse", pulse_synth),
        ("Pulse Moog", pulse_moog)
    ];
    ChooserTable::from(&synth_funcs)
}

pub fn pulse_moog(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(moogify!(env_pulse_sin(pitch)) * adsr_2(note_m) * volume)
}

pub fn triangle_moog(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(moogify!(env_triangle(pitch)) * adsr_2(note_m) * volume)
}

fn env_triangle(
    pitch: f64,
) -> An<
    Pipe<
        f64,
        Constant<UInt<UTerm, typenum::bit::B1>, f64>,
        WaveSynth<'static, f64, UInt<UTerm, typenum::B1>>,
    >,
> {
    constant(pitch) >> triangle()
}

// Pulse envelope copied from fundsp/examples/beep.rs
fn env_pulse_sin(
    pitch: f64,
) -> An<Pipe<f64, Envelope<f64, f64, impl Fn(f64) -> (f64, f64) + Sized, (f64, f64)>, PulseWave<f64>>>
{
    envelope(move |t| (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))) >> pulse()
}

pub fn triangle_synth(
    pitch: u8,
    volume: u8,
    note_m: Arc<AtomicCell<SoundMsg>>,
) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(env_triangle(pitch) * adsr_2(note_m) * volume)
}

pub fn pulse_synth(pitch: u8, volume: u8, note_m: Arc<AtomicCell<SoundMsg>>) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(env_pulse_sin(pitch) * adsr_2(note_m) * volume)
}

fn adsr_1(
    note_m: Arc<AtomicCell<SoundMsg>>,
) -> An<
    Unop<
        f64,
        Envelope<f64, f64, impl Fn(f64) -> f64 + Sized, f64>,
        FrameMulScalar<UInt<UTerm, typenum::B1>, f64>,
    >,
> {
    2.0 * adsr_live(0.1, 0.8, 0.2, 0.2, note_m)
}

fn adsr_2(
    note_m: Arc<AtomicCell<SoundMsg>>,
) -> An<
    Unop<
        f64,
        Envelope<f64, f64, impl Fn(f64) -> f64 + Sized, f64>,
        FrameMulScalar<UInt<UTerm, typenum::B1>, f64>,
    >,
> {
    2.0 * adsr_live(0.2, 0.2, 0.4, 0.2, note_m)
}