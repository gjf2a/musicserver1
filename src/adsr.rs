use crossbeam_utils::atomic::AtomicCell;
use fundsp::hacker::{lerp, lfo, An, Envelope};
use std::fmt::Debug;
use std::sync::Arc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SoundMsg {
    Play,
    Release,
    Finished,
}

/// Credit: <https://stackoverflow.com/a/73205224/906268> for showing how to write the return type
pub fn adsr_live(
    attack: f64,
    decay: f64,
    sustain: f64,
    release: f64,
    note_m: Arc<AtomicCell<SoundMsg>>,
) -> An<Envelope<f64, f64, impl Fn(f64) -> f64 + Sized, f64>> {
    adsr(attack, decay, sustain, release, None, note_m)
}

pub fn adsr_fixed(
    attack: f64,
    decay: f64,
    sustain_time: f64,
    sustain_level: f64,
    release: f64,
    note_m: Arc<AtomicCell<SoundMsg>>,
) -> An<Envelope<f64, f64, impl Fn(f64) -> f64 + Sized, f64>> {
    adsr(
        attack,
        decay,
        sustain_level,
        release,
        Some(attack + decay + sustain_time),
        note_m,
    )
}

fn adsr(
    attack: f64,
    decay: f64,
    sustain: f64,
    release: f64,
    release_start: Option<f64>,
    note_m: Arc<AtomicCell<SoundMsg>>,
) -> An<Envelope<f64, f64, impl Fn(f64) -> f64 + Sized, f64>> {
    let adsr = AtomicCell::new(Adsr {
        attack,
        decay,
        sustain,
        release,
        release_start,
    });
    lfo(move |t| {
        if note_m.load() == SoundMsg::Release {
            let mut adsr_inner = adsr.load();
            adsr_inner.release(t);
            adsr.store(adsr_inner);
            note_m.store(SoundMsg::Play);
        }
        match adsr.load().volume(t) {
            Some(v) => v,
            None => {
                note_m.store(SoundMsg::Finished);
                0.0
            }
        }
    })
}

#[derive(Copy, Clone, Debug)]
struct Adsr {
    attack: f64,
    decay: f64,
    sustain: f64,
    release: f64,
    release_start: Option<f64>,
}

impl Adsr {
    fn release(&mut self, time_now: f64) {
        self.release_start = Some(time_now);
    }

    fn volume(&self, time: f64) -> Option<f64> {
        match self.release_start {
            None => Some(self.ads(time)),
            Some(release_start) => {
                if time < release_start {
                    Some(self.ads(time))
                } else {
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

    fn ads(&self, time: f64) -> f64 {
        if time < self.attack {
            lerp(0.0, 1.0, time / self.attack)
        } else if time - self.attack < self.decay {
            lerp(1.0, self.sustain, (time - self.attack) / self.decay)
        } else {
            self.sustain
        }
    }
}
