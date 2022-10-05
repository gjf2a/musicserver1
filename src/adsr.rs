use fundsp::hacker::{An, Envelope, lerp, lfo};
use std::time::Instant;
use std::fmt::{Debug, Formatter};

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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SoundMsg {
    Play, Release, Stop
}

#[derive(Copy, Clone)]
pub struct Adsr {
    attack: f64, decay: f64, sustain: f64, release: f64, release_start: Option<Instant>
}

impl Adsr {
    pub fn new(attack: f64, decay: f64, sustain: f64, release: f64) -> Self {
        Adsr {attack, decay, sustain, release, release_start: None}
    }

    pub fn release(&mut self) {
        self.release_start = Some(Instant::now());
    }

    pub fn volume(&self, time: f64) -> Option<f64> {
        match self.release_start {
            None => {
                if time < self.attack {
                    Some(lerp(0.0, 1.0, time / self.attack))
                } else if time - self.attack < self.decay {
                    Some(lerp(1.0, self.sustain, (time - self.attack) / self.decay))
                } else {
                    Some(self.sustain)
                }
            }
            Some(release_start) => {
                let release_time = time - Self::elapsed(release_start);
                if release_time > self.release {
                    None
                } else {
                    Some(lerp(self.sustain, 0.0, release_time / self.release))
                }
            }
        }
    }

    fn elapsed(instant: Instant) -> f64 {instant.elapsed().as_secs_f64()}
}

impl Debug for Adsr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Adsr {{ attack: {} decay: {} sustain: {} release: {} release_start: {:?} }}",
               self.attack, self.decay, self.sustain, self.release,
               self.release_start.map(Self::elapsed)
        )
    }
}
