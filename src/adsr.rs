#[macro_export]
macro_rules! adsr_fixed {
    ($attack:expr, $decay:expr, $sustain:expr, $sustain_level:expr, $release:expr) => {
        lfo(move |time_s| {
            let mut time_s = time_s;
            if time_s < $attack {
                return fundsp::hacker::lerp(0.0, 1.0, time_s / $attack);
            }
            time_s -= $attack;
            if time_s < $decay {
                return fundsp::hacker::lerp(1.0, $sustain_level, time_s / $decay);
            }
            time_s -= $decay;
            if time_s < $sustain {
                return $sustain_level;
            }
            time_s -= $release;
            if time_s < $release {
                return fundsp::hacker::lerp($sustain_level, 0.0, time_s / $release);
            }
            0.0
        })
    }
}