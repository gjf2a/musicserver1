use fundsp::hacker::{An, Envelope, lerp, lfo};

/// Credit: https://stackoverflow.com/a/73205224/906268 for showing how to write the return type
pub fn adsr_fixed(attack: f64, decay: f64, sustain: f64, sustain_level: f64, release: f64) -> An<Envelope<f64, f64, impl Fn(f64)->f64 + 'static, f64>> {
    lfo(move |time_s| adsr_level(time_s, attack, decay, sustain, sustain_level, release))
}

pub fn adsr_level(time_s: f64, attack: f64, decay: f64, sustain: f64, sustain_level: f64, release: f64) -> f64 {
    let mut time_s = time_s;
    if time_s < attack {
        return lerp(0.0, 1.0, time_s / attack);
    }
    time_s -= attack;
    if time_s < decay {
        return lerp(1.0, sustain_level, time_s / decay);
    }
    time_s -= decay;
    if time_s < sustain {
        return sustain_level;
    }
    time_s -= release;
    if time_s < release {
        return lerp(sustain_level, 0.0, time_s / release);
    }
    0.0
}
