use fundsp::combinator::An;
use fundsp::envelope::Envelope;
use fundsp::hacker::{envelope, envelope2, lerp11, pulse, sin_hz, triangle, Pipe, WaveSynth, moog_q, xerp11, constant, Constant};
use fundsp::prelude::{AudioUnit64, PulseWave};
use typenum::{UInt, UTerm};
use crate::runtime::ChooserTable;
use crate::synth_output::{convert_midi, SynthTable};

// Moog envelope adapted from fundsp/examples/beep.rs
// I'm using a macro because I could not figure out how to write down the type for $an, which is
// an An<AudioNode> of some sort.
#[macro_export]
macro_rules! moogify {
    ($an:expr) => (($an | envelope(|t| xerp11(110.0, 11000.0, sin_hz(0.60, t)))) >> moog_q(0.6))
}

pub fn make_synth_table() -> SynthTable {
    let synth_funcs: Vec<(&str, Box<dyn AudioUnit64>)> = vec![
        ("Triangle", Box::new(triangle())),
        ("Pulse", Box::new(envelope2(move |t, p| (p, lerp11(0.01, 0.99, sin_hz(0.05, t)))) >> pulse()))
    ];
    ChooserTable::from(&synth_funcs)
}

pub fn pulse_moog(pitch: u8, volume: u8) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(moogify!(env_pulse_sin(pitch)) * volume)
}

pub fn triangle_moog(pitch: u8, volume: u8) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(moogify!(env_triangle(pitch)) * volume)
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
) -> An<Pipe<f64, Envelope<f64, f64, impl Fn(f64) -> (f64, f64) + Sized + Clone, (f64, f64)>, PulseWave<f64>>>
{
    envelope(move |t| (pitch, lerp11(0.01, 0.99, sin_hz(0.05, t)))) >> pulse()
}

pub fn triangle_synth(pitch: u8, volume: u8,) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(env_triangle(pitch) * volume)
}

pub fn pulse_synth(pitch: u8, volume: u8) -> Box<dyn AudioUnit64> {
    let (pitch, volume) = convert_midi(pitch, volume);
    Box::new(env_pulse_sin(pitch) * volume)
}

