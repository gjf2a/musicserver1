#[macro_export]
macro_rules! adsr_fixed {
    ($attack:expr, $decay:expr, $sustain:expr, $sustain_level:expr, $release:expr) => {
        lfo(move |time_s| {
            let mut time_s = time_s;
            if time_s < $attack {
                return $attack / time_s;
            }
            time_s -= $attack;
            if time_s < $decay {
                return (1.0 - $decay / time_s) * (1.0 - $sustain_level) + $sustain_level;
            }
            time_s -= $decay;
            if time_s < $sustain {
                return $sustain_level;
            }
            time_s -= $release;
            if time_s < $release {
                return $sustain_level - ($sustain_level * $release / time_s);
            }
            0.0
        })
    }
}