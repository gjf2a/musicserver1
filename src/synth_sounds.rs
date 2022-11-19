use crate::runtime::ChooserTable;
use crate::synth_output::{SynthTable, SynthType};
use fundsp::combinator::An;
use fundsp::envelope::Envelope2;
use fundsp::hacker::{envelope, envelope2, lerp11, moog_q, pulse, sin_hz, triangle, xerp11, Pipe};
use fundsp::prelude::PulseWave;

// Moog envelope adapted from fundsp/examples/beep.rs
// I'm using a macro because it is quicker to write down than using Net64.
#[macro_export]
macro_rules! moogify {
    ($an:expr) => {
        ($an | envelope(|t| xerp11(1100.0, 11000.0, sin_hz(0.15, t)))) >> moog_q(0.6)
    };
}

const ADSR1: (f64, f64, f64, f64) = (0.1, 0.2, 0.4, 0.2);
const ADSR2: (f64, f64, f64, f64) = (0.0, 0.0, 1.0, 0.0);

/// All sounds should have one input (the pitch and the ADSR control) and one output (the sound wave).
/// The ADSR values should be supplied in order as well.
pub fn make_synth_table() -> SynthTable {
    let synth_funcs: Vec<(&str, SynthType)> = vec![
        ("80s Beep", (Box::new(triangle()), ADSR2)),
        ("Triangle", (Box::new(triangle()), ADSR1)),
        ("Pulse", (Box::new(sin_pulse()), ADSR1)),
        ("Triangle Moog", (Box::new(moogify!(triangle())), ADSR1)),
        ("Pulse Moog", (Box::new(moogify!(sin_pulse())), ADSR1)),
    ];
    ChooserTable::from(&synth_funcs)
}

fn sin_pulse() -> An<
    Pipe<
        f64,
        Envelope2<f64, f64, impl Fn(f64, f64) -> (f64, f64) + Sized + Clone, (f64, f64)>,
        PulseWave<f64>,
    >,
> {
    envelope2(move |t, p| (p, lerp11(0.01, 0.99, sin_hz(0.05, t)))) >> pulse()
}
