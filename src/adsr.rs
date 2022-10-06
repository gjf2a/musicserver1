use fundsp::hacker::{An, Envelope, lerp, lfo};
use std::fmt::Debug;
use std::sync::Arc;
use crossbeam_utils::atomic::AtomicCell;

/// Credit: https://stackoverflow.com/a/73205224/906268 for showing how to write the return type
pub fn adsr_live(attack: f64, decay: f64, sustain: f64, release: f64, note_m: Arc<AtomicCell<SoundMsg>>) -> An<Envelope<f64, f64, impl Fn(f64)->f64 + 'static, f64>> {
    let adsr = AtomicCell::new(Adsr::live(attack, decay, sustain, release));
    lfo(move |t| {
        if note_m.load() == SoundMsg::Release {
            let mut adsr_inner = adsr.load();
            adsr_inner.release(t);
            adsr.store(adsr_inner);
            note_m.store(SoundMsg::Play);
        }
        match adsr.load().volume(t) {
            Some(v) => v,
            None => {note_m.store(SoundMsg::Stop); 0.0}
        }
    })
}

pub fn adsr_fixed(attack: f64, decay: f64, sustain_time: f64, sustain_level: f64, release: f64, note_m: Arc<AtomicCell<SoundMsg>>) -> An<Envelope<f64, f64, impl Fn(f64)->f64 + 'static, f64>> {
    let adsr = AtomicCell::new(Adsr::fixed(attack, decay, sustain_time, sustain_level, release));
    lfo(move |t| {
        match adsr.load().volume(t) {
            Some(v) => v,
            None => {note_m.store(SoundMsg::Stop); 0.0}
        }
    })
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SoundMsg {
    Play, Release, Stop
}

#[derive(Copy, Clone, Debug)]
pub struct Adsr {
    attack: f64, decay: f64, sustain: f64, release: f64, release_start: Option<f64>
}

impl Adsr {
    pub fn live(attack: f64, decay: f64, sustain: f64, release: f64) -> Self {
        Adsr {attack, decay, sustain, release, release_start: None}
    }

    pub fn fixed(attack: f64, decay: f64, sustain_time: f64, sustain_level: f64, release: f64) -> Self {
        Adsr {attack, decay, sustain: sustain_level, release, release_start: Some(attack + decay + sustain_time)}
    }

    pub fn release(&mut self, time_now: f64) {
        self.release_start = Some(time_now);
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
                let release_time = time - release_start;
                if release_time > self.release {
                    None
                } else {
                    Some(lerp(self.sustain, 0.0, release_time / self.release))
                }
            }
        }
    }
}
