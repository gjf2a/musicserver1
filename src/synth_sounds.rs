use fundsp::combinator::An;
use fundsp::envelope::Envelope2;
use fundsp::hacker::{envelope, envelope2, lerp11, pulse, sin_hz, triangle, Pipe, moog_q, xerp11};
use fundsp::prelude::{AudioUnit64, PulseWave};
use crate::runtime::ChooserTable;
use crate::synth_output::SynthTable;

// Moog envelope adapted from fundsp/examples/beep.rs
// I'm using a macro because it is quicker to write down than using Net64.
#[macro_export]
macro_rules! moogify {
    ($an:expr) => (($an | envelope(|t| xerp11(1100.0, 11000.0, sin_hz(0.15, t)))) >> moog_q(0.6))
}

/// All sounds should have one input (the pitch) and one output (the sound wave).
pub fn make_synth_table() -> SynthTable {
    let synth_funcs: Vec<(&str, Box<dyn AudioUnit64>)> = vec![
        ("Triangle", Box::new(triangle())),
        ("Pulse", Box::new(sin_pulse())),
        ("Triangle Moog", Box::new(moogify!(triangle()))),
        ("Pulse Moog", Box::new(moogify!(sin_pulse())))
    ];
    ChooserTable::from(&synth_funcs)
}

fn sin_pulse() -> An<Pipe<f64, Envelope2<f64, f64, impl Fn(f64,f64) -> (f64, f64) + Sized + Clone, (f64, f64)>, PulseWave<f64>>> {
    envelope2(move |t, p| (p, lerp11(0.01, 0.99, sin_hz(0.05, t)))) >> pulse()
}