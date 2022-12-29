use std::collections::VecDeque;

use musicserver1::analyzer::MidiByte;

pub fn keep_only<F: Fn(char) -> bool>(s: &str, check: F) -> String {
    s.chars().map(|c| if check(c) { c } else { ' ' }).collect()
}

pub fn keep_notes(s: &str) -> String {
    keep_only(s, |c| c.is_digit(10) || c == '.')
}

pub fn note_nums_from(s: &str) -> VecDeque<String> {
    keep_notes(s)
        .split_whitespace()
        .map(|p| p.to_owned())
        .collect()
}

fn main() {
    let mut nums = note_nums_from(S3);
    while nums.len() > 0 {
        let pitch = nums.pop_front().unwrap().parse::<MidiByte>().unwrap();
        let duration = nums.pop_front().unwrap().parse::<f64>().unwrap();
        let velocity = nums.pop_front().unwrap().parse::<MidiByte>().unwrap();
        println!("Note::new({pitch}, {duration}, {velocity}),");
    }
}

const S1: &'static str = "Note {
    pitch: 60,
    duration: OrderedFloat(0.487445772),
    velocity: 92,
},
Note {
    pitch: 60,
    duration: OrderedFloat(0.377421752),
    velocity: 0,
},
Note {
    pitch: 60,
    duration: OrderedFloat(0.289316858),
    velocity: 93,
},
Note {
    pitch: 60,
    duration: OrderedFloat(0.005971111),
    velocity: 0,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.248933836),
    velocity: 102,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.05767016),
    velocity: 0,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.25962179),
    velocity: 113,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.229479448),
    velocity: 0,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.317320844),
    velocity: 4,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.042830378),
    velocity: 0,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.582655121),
    velocity: 70,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.50379576),
    velocity: 0,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.250825755),
    velocity: 106,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.017210736),
    velocity: 0,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.272029135),
    velocity: 100,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.027428442),
    velocity: 0,
},
Note {
    pitch: 65,
    duration: OrderedFloat(1.126041184),
    velocity: 99,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.548192689),
    velocity: 99,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.273066185),
    velocity: 99,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.60045823),
    velocity: 117,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.450277594),
    velocity: 0,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.269494265),
    velocity: 85,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.003552609),
    velocity: 0,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.267746147),
    velocity: 96,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.016828202),
    velocity: 0,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.382390025),
    velocity: 123,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.128571533),
    velocity: 0,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.699718069),
    velocity: 113,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.126759354),
    velocity: 0,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.867493649),
    velocity: 117,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.46433006),
    velocity: 0,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.268483555),
    velocity: 106,
},
Note {
    pitch: 60,
    duration: OrderedFloat(1.323782698),
    velocity: 106,
},
Note {
    pitch: 60,
    duration: OrderedFloat(0.247095603),
    velocity: 100,
},
Note {
    pitch: 60,
    duration: OrderedFloat(0.026361804),
    velocity: 0,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.312539865),
    velocity: 30,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.008570104),
    velocity: 0,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.267542603),
    velocity: 100,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.05672056),
    velocity: 0,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.60457732),
    velocity: 110,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.537155291),
    velocity: 0,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.271879248),
    velocity: 113,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.06866826),
    velocity: 0,
},
Note {
    pitch: 67,
    duration: OrderedFloat(0.253815119),
    velocity: 88,
},
Note {
    pitch: 67,
    duration: OrderedFloat(0.10896619),
    velocity: 0,
},
Note {
    pitch: 65,
    duration: OrderedFloat(1.4822801190000001),
    velocity: 78,
},
Note {
    pitch: 67,
    duration: OrderedFloat(0.362781309),
    velocity: 78,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.202632925),
    velocity: 78,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.651313167),
    velocity: 118,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.412422427),
    velocity: 0,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.315570477),
    velocity: 86,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.032453773),
    velocity: 0,
},
Note {
    pitch: 57,
    duration: OrderedFloat(1.254315847),
    velocity: 92,
},
Note {
    pitch: 65,
    duration: OrderedFloat(0.321109969),
    velocity: 92,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.64096164),
    velocity: 117,
},
Note {
    pitch: 64,
    duration: OrderedFloat(0.485079544),
    velocity: 0,
},
Note {
    pitch: 55,
    duration: OrderedFloat(0.530547672),
    velocity: 94,
},
Note {
    pitch: 55,
    duration: OrderedFloat(0.017645017),
    velocity: 0,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.263664442),
    velocity: 125,
},
Note {
    pitch: 62,
    duration: OrderedFloat(0.009401743),
    velocity: 0,
},
Note {
    pitch: 60,
    duration: OrderedFloat(0.670999911),
    velocity: 111,
},
Note {
    pitch: 60,
    duration: OrderedFloat(1.5000003039999998),
    velocity: 0,
},
";

const S2: &'static str = "Note {
    pitch: 72,
    duration: OrderedFloat(0.369792827),
    velocity: 127,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.061621093),
    velocity: 0,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.329988672),
    velocity: 127,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.127228427),
    velocity: 0,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.3714564),
    velocity: 99,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.085686124),
    velocity: 0,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.330959396),
    velocity: 127,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.159859305),
    velocity: 0,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.314343867),
    velocity: 76,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.026874508),
    velocity: 0,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.037653197),
    velocity: 12,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.044087338),
    velocity: 0,
},
Note {
    pitch: 75,
    duration: OrderedFloat(0.024298299),
    velocity: 77,
},
Note {
    pitch: 75,
    duration: OrderedFloat(0.000002524),
    velocity: 0,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.280279047),
    velocity: 80,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.178904154),
    velocity: 0,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.323056836),
    velocity: 89,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.1009156),
    velocity: 0,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.330098162),
    velocity: 127,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.10557298),
    velocity: 0,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.300934497),
    velocity: 80,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.00000287),
    velocity: 0,
},
Note {
    pitch: 73,
    duration: OrderedFloat(0.022893581),
    velocity: 20,
},
Note {
    pitch: 73,
    duration: OrderedFloat(0.044463414),
    velocity: 0,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.394669786),
    velocity: 101,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.078055973),
    velocity: 0,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.025097277),
    velocity: 91,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.00000256),
    velocity: 0,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.332283936),
    velocity: 92,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.113469215),
    velocity: 0,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.345122384),
    velocity: 127,
},
Note {
    pitch: 77,
    duration: OrderedFloat(1.5000002829999999),
    velocity: 0,
},
";

const S3: &'static str = "Note {
    pitch: 72,
    duration: OrderedFloat(0.369792827),
    velocity: 127,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.061621093),
    velocity: 0,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.329988672),
    velocity: 127,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.127228427),
    velocity: 0,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.3714564),
    velocity: 99,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.085686124),
    velocity: 0,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.330959396),
    velocity: 127,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.159859305),
    velocity: 0,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.314343867),
    velocity: 76,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.026874508),
    velocity: 0,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.280279047),
    velocity: 80,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.178904154),
    velocity: 0,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.323056836),
    velocity: 89,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.1009156),
    velocity: 0,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.330098162),
    velocity: 127,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.10557298),
    velocity: 0,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.300934497),
    velocity: 80,
},
Note {
    pitch: 72,
    duration: OrderedFloat(0.00000287),
    velocity: 0,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.394669786),
    velocity: 101,
},
Note {
    pitch: 74,
    duration: OrderedFloat(0.078055973),
    velocity: 0,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.332283936),
    velocity: 92,
},
Note {
    pitch: 76,
    duration: OrderedFloat(0.113469215),
    velocity: 0,
},
Note {
    pitch: 77,
    duration: OrderedFloat(0.345122384),
    velocity: 127,
},
Note {
    pitch: 77,
    duration: OrderedFloat(1.5000002829999999),
    velocity: 0,
},
";
