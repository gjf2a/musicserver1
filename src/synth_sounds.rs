use fundsp::hacker::{lerp11, lfo, lfo2, pulse, sin_hz, triangle};
use fundsp::prelude::AudioUnit64;
use std::sync::Arc;
use crate::{func_vec, SynthTable, SynthFunc};

pub fn make_synth_table() -> SynthTable {
    let synth_funcs = func_vec![SynthFunc,
            ("Sine Pulse", sine_pulse),
            ("ADSR 1", expr1),
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

#[macro_export]
macro_rules! adsr_fixed {
    ($attack:expr, $decay:expr, $sustain:expr, $sustain_level:expr, $release:expr) => {
        lfo(move |t| {
            if t < $attack {
                $attack / t
            } else if t < $attack + $decay {
                (1.0 - ((t - $attack) / $decay)) * (1.0 - $sustain_level) + $sustain_level
            } else if t < $attack + $decay + $sustain {
                $sustain_level
            } else if t < $attack + $decay + $sustain + $release {
                1.0 - ((t - $sustain) / $release)
            } else {
                0.0
            }
        })
    }
}

pub fn expr1(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |_t| pitch) >> triangle() * adsr_fixed!(0.2, 0.2, 0.4, 0.4, 0.2) * volume)
}

#[cfg(test)]
mod tests {
    use fundsp::hacker::lerp;

    #[test]
    fn see_lerp() {
        println!("Start");
        let a = 10.0;
        let b = 1.0;
        for (i, v) in (0..10).map(|n| lerp(a, b, n as f64/10.0)).enumerate() {
            println!("{i} {v} {}", a + (b - a) * i as f64/10.0);
        }
        println!("End");
        assert!(false)
    }
}