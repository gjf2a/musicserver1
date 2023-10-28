use std::{
    collections::{BTreeSet, BinaryHeap},
    sync::Arc,
    time::Instant,
};

use crossbeam_queue::SegQueue;
use midi_fundsp::{io::SynthMsg, NUM_MIDI_VALUES};
use midi_msg::MidiMsg;
use ordered_float::OrderedFloat;

use crate::analyzer::{MidiByte, Note};

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

fn duration_since(start: Instant) -> OrderedFloat<f64> {
    duration_between(start, Instant::now())
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

    fn to_note(&self, total_duration: OrderedFloat<f64>) -> Note {
        let (note, velocity) = self.note_velocity();
        let duration = self.end.unwrap_or(total_duration) - self.time;
        Note::new(note, duration.into(), velocity)
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
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.time.partial_cmp(&other.time) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.end.partial_cmp(&other.end)
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

    pub fn chords(&self) -> Vec<Chord> {
        assert!(self.len() >= 2);
        let mut playback = self.playback();
        let mut result = vec![];
        let mut current_notes = BTreeSet::new();
        let mut prev = None;
        while let Some(cmd) = playback.next() {
            let pitch = cmd.note_velocity().0;
            if cmd.is_note_on() {
                current_notes.insert(pitch);
            } else {
                current_notes.remove(&pitch);
            }
            if let Some((prev_notes, prev_time)) = prev {
                result.push(Chord {
                    start: prev_time,
                    duration: cmd.time - prev_time,
                    notes: prev_notes,
                });
            }
            prev = Some((current_notes.iter().copied().collect::<Vec<_>>(), cmd.time));
        }
        result
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Chord {
    start: OrderedFloat<f64>,
    duration: OrderedFloat<f64>,
    notes: Vec<MidiByte>,
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
        if let Some(p) = self.pending.peek() {
            let popped = self.pending.pop().unwrap();
            if let Some(off) = popped.off_cmd() {
                self.pending.push(off);
            }
            return Some(popped);
        }
        None
    }
}

struct MidiCmdCandidate {
    msg: SynthMsg,
    start: Instant,
    end: Option<OrderedFloat<f64>>,
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
        self.end = Some(duration_since(self.start));
    }

    fn timed_note(&self, start: Instant) -> TimedMidiCmd {
        TimedMidiCmd {
            time: duration_between(start, self.start),
            msg: self.msg.clone(),
            end: Some(self.end.unwrap_or(duration_since(self.start))),
        }
    }
}

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
        if let Some(s) = self.input2recorder.pop() {
            self.recorder2output.push(s.clone());
            if let Some((note, velocity)) = s.note_velocity() {
                self.finalize_pitch(note);
                if velocity > 0 {
                    self.pitch2candidate[note as usize] = Some(self.notes.len());
                    self.notes.push(MidiCmdCandidate::new(&s));
                }
            }
            true
        } else {
            false
        }
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
