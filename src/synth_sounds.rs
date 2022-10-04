use fundsp::hacker::{lerp11, lfo, pulse, sin_hz, triangle};
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

pub struct AdsrFixed {
    pub attack: f64, pub decay: f64, pub sustain: f64, pub sustain_level: f64, pub release: f64
}

impl AdsrFixed {
    pub fn level(&self, time_s: f64) -> f64 {
        let mut time_s = time_s;
        if time_s < self.attack {
            return self.attack / time_s;
        }
        time_s -= self.attack;
        if time_s < self.decay {
            return (1.0 - self.decay / time_s) * (1.0 - self.sustain_level) + self.sustain_level;
        }
        time_s -= self.decay;
        if time_s < self.sustain {
            return self.sustain_level;
        }
        time_s -= self.release;
        if time_s < self.release {
            return self.sustain_level - (self.sustain_level * self.release / time_s);
        }
        0.0
    }
}

#[macro_export]
macro_rules! adsr_fixed {
    ($adsrf:expr) => {
        lfo(move |t| {$adsrf.level(t)})
    }
}

pub fn expr1(pitch: f64, volume: f64) -> Box<dyn AudioUnit64> {
    Box::new(lfo(move |_t| pitch) >> triangle() * adsr_fixed!(AdsrFixed {attack: 0.2, decay: 0.2, sustain: 0.4, sustain_level: 0.4, release: 0.2}) * volume)
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