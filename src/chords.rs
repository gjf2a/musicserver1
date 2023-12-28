use std::{
    collections::{BTreeSet, BinaryHeap, BTreeMap},
    sync::Arc,
    time::Instant,
};

use crossbeam_queue::SegQueue;
use midi_fundsp::{io::SynthMsg, NUM_MIDI_VALUES};
use midi_msg::MidiMsg;
use ordered_float::OrderedFloat;

use crate::analyzer::{MidiByte, MusicMode, USIZE_NOTES_PER_OCTAVE};

pub struct ChordRecorder {
    input2recorder: Arc<SegQueue<SynthMsg>>,
    recorder2output: Arc<SegQueue<SynthMsg>>,
    pitch2candidate: [Option<usize>; NUM_MIDI_VALUES],
    notes: Vec<MidiCmdCandidate>,
    start: Instant,
}

impl ChordRecorder {
    pub fn new(
        input2recorder: Arc<SegQueue<SynthMsg>>,
        recorder2output: Arc<SegQueue<SynthMsg>>,
    ) -> Self {
        Self {
            input2recorder,
            recorder2output,
            pitch2candidate: [None; NUM_MIDI_VALUES],
            notes: vec![],
            start: Instant::now(),
        }
    }

    pub fn try_next_input(&mut self) -> bool {
        self.input2recorder.pop().map_or(false, |s| {
            self.recorder2output.push(s.clone());
            if let Some((note, velocity)) = s.note_velocity() {
                self.finalize_pitch(note);
                if velocity > 0 {
                    self.pitch2candidate[note as usize] = Some(self.notes.len());
                    self.notes.push(MidiCmdCandidate::new(&s));
                }
            }
            true
        })
    }

    fn finalize_pitch(&mut self, pitch: u8) {
        if let Some(i) = self.pitch2candidate[pitch as usize] {
            self.notes[i].finalize();
            self.pitch2candidate[pitch as usize] = None;
        }
    }

    pub fn recording(&self) -> PolyphonicRecording {
        PolyphonicRecording::new(
            self.notes
                .iter()
                .map(|n| n.timed_note(self.start))
                .collect(),
        )
    }
}

#[derive(Clone, Debug)]
struct MidiCmdCandidate {
    msg: SynthMsg,
    start: Instant,
    end: Option<Instant>,
}

impl MidiCmdCandidate {
    fn new(msg: &SynthMsg) -> Self {
        Self {
            msg: msg.clone(),
            start: Instant::now(),
            end: None,
        }
    }

    fn finalize(&mut self) {
        self.end = Some(Instant::now());
    }

    fn timed_note(&self, start: Instant) -> TimedMidiCmd {
        TimedMidiCmd {
            time: duration_between(start, self.start),
            msg: self.msg.clone(),
            end: Some(duration_between(start, self.end.unwrap_or(Instant::now()))),
        }
    }
}

fn note_on_to_off(input: &SynthMsg) -> SynthMsg {
    if let MidiMsg::ChannelVoice { channel, msg } = input.msg {
        if let midi_msg::ChannelVoiceMsg::NoteOn { note, velocity: _ } = msg {
            return SynthMsg {
                speaker: input.speaker,
                msg: MidiMsg::ChannelVoice {
                    channel,
                    msg: midi_msg::ChannelVoiceMsg::NoteOff { note, velocity: 0 },
                },
            };
        }
    }
    input.clone()
}

fn duration_between(early: Instant, later: Instant) -> OrderedFloat<f64> {
    OrderedFloat(later.duration_since(early).as_secs_f64())
}

#[derive(Clone, Debug)]
pub struct TimedMidiCmd {
    time: OrderedFloat<f64>,
    msg: SynthMsg,
    end: Option<OrderedFloat<f64>>,
}

impl TimedMidiCmd {
    fn is_note_on(&self) -> bool {
        self.off_cmd().is_some()
    }

    fn note_velocity(&self) -> (MidiByte, MidiByte) {
        let (note, velocity) = self.msg.note_velocity().unwrap();
        (note as MidiByte, velocity as MidiByte)
    }

    fn duration(&self) -> Option<OrderedFloat<f64>> {
        self.end.map(|e| e - self.time)
    }

    fn off_cmd(&self) -> Option<Self> {
        self.end.map(|time| Self {
            time,
            msg: note_on_to_off(&self.msg),
            end: None,
        })
    }
}

impl PartialEq for TimedMidiCmd {
    fn eq(&self, other: &Self) -> bool {
        self.time == other.time && self.end == other.end
    }
}

impl Eq for TimedMidiCmd {}

impl PartialOrd for TimedMidiCmd {
    /// This is designed to sort in reverse order of start time.
    /// If the self `TimedMidiCmd` starts earlier than `other`, this returns Ordering::Greater.
    /// The purpose behind this is to use a max-heap to get the earliest possible `TimedMidiCmd`.
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match other.time.partial_cmp(&self.time) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        other.end.partial_cmp(&self.end)
    }
}

impl Ord for TimedMidiCmd {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub struct PolyphonicRecording {
    notes: Vec<TimedMidiCmd>,
    total_time: OrderedFloat<f64>,
}

impl PolyphonicRecording {
    pub fn new(notes: Vec<TimedMidiCmd>) -> Self {
        let total_time = notes.iter().filter_map(|n| n.duration()).sum();
        Self { notes, total_time }
    }

    pub fn playback(&self) -> PolyphonicPlayback {
        PolyphonicPlayback {
            pending: self.notes.iter().cloned().collect(),
        }
    }

    pub fn len(&self) -> usize {
        self.notes.len()
    }

    pub fn chords(&self, min_chord_duration: f64) -> Vec<Chord> {
        let mut playback = self.playback();
        let mut result = vec![];
        let mut current_notes = BTreeMap::new();
        let mut prev = None;
        while let Some(cmd) = playback.next() {
            let (pitch, velocity) = cmd.note_velocity();
            if cmd.is_note_on() {
                current_notes.insert(pitch, velocity);
            } else {
                current_notes.remove(&pitch);
            }
            let mut prev_prev = None;
            std::mem::swap(&mut prev, &mut prev_prev);
            push_next_chord(&mut result, cmd.time, prev_prev, min_chord_duration);
            prev = Some((current_notes.iter().map(|(n, v)| (*n, *v)).collect::<Vec<_>>(), cmd.time));
        }
        push_next_chord(&mut result, self.total_time, prev, min_chord_duration);
        result
    }
}

fn push_next_chord(
    result: &mut Vec<Chord>,
    current_time: OrderedFloat<f64>,
    prev: Option<(Vec<(MidiByte,MidiByte)>, OrderedFloat<f64>)>,
    min_chord_duration: f64,
) {
    if let Some((prev_notes, prev_time)) = prev {
        let duration = current_time - prev_time;
        if duration > OrderedFloat(min_chord_duration) {
            result.push(Chord {
                start: prev_time,
                duration,
                notes_velocities: prev_notes,
            });
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Chord {
    start: OrderedFloat<f64>,
    duration: OrderedFloat<f64>,
    notes_velocities: Vec<(MidiByte,MidiByte)>,
}

impl Chord {
    pub fn note_weight_vector(&self) -> Option<[f64; USIZE_NOTES_PER_OCTAVE]> {
        if self.notes_velocities.len() > 0 {
            let mut result = [0.0; USIZE_NOTES_PER_OCTAVE];
            for (n,_) in self.notes_velocities.iter() {
                result[*n as usize % USIZE_NOTES_PER_OCTAVE] += self.duration.0;
            }
            Some(result)
        } else {
            None
        }
    }

    pub fn best_scale_for(&self) -> Option<MusicMode> {
        self.note_weight_vector().map(|weights| MusicMode::best_scale_for(&weights))
    }
}

pub struct PolyphonicPlayback {
    pending: BinaryHeap<TimedMidiCmd>,
}

impl PolyphonicPlayback {
    pub fn play(&mut self, to_output: Arc<SegQueue<SynthMsg>>) {
        if let Some(cmd) = self.next() {
            to_output.push(cmd.msg);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.pending.is_empty()
    }

    pub fn next(&mut self) -> Option<TimedMidiCmd> {
        self.pending.pop().map(|popped| {
            if let Some(off) = popped.off_cmd() {
                self.pending.push(off);
            }
            popped
        })
    }
}
