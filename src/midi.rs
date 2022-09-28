// I experimented with several exising MIDI crates, but none were suitable:
// * midly and midi-msg both crashed on my USB inputs.
// * helgoboss-midi was incomprehensible
// * midi-control is LGPL
// * midi-types is poorly documented
// * apres is more about files than live events

// Midi message types: http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html#BMA1_1

const NOTE_ON: u8 = 9;
const NOTE_OFF: u8 = 8;
const PITCH_BEND: u8 = 14;
const AFTERTOUCH: u8 = 10;
const CONTROL_CHANGE: u8 = 11;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct MidiBytes {
    cmd: u8, channel: u8, arg1: u8, arg2: u8
}

impl MidiBytes {
    pub fn from(input: &[u8]) -> Self {
        MidiBytes {cmd: input[0], channel: input[1], arg1: input[2], arg2: input[3]}
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum MidiMsg {
    NoteVelocity(u8, u8), PitchBend(u16), AftertouchNotePressure(u8,u8), ControlChangeNumValue(u8,u8)
}

impl MidiMsg {
    pub fn from_bytes(input: MidiBytes) -> Option<Self> {
        match input.cmd {
            NOTE_ON | NOTE_OFF => Some(MidiMsg::NoteVelocity(input.arg1, input.arg2)),
            PITCH_BEND         => Some(MidiMsg::PitchBend(Self::pitch_bend_value(input.arg1, input.arg2))),
            AFTERTOUCH         => Some(MidiMsg::AftertouchNotePressure(input.arg1, input.arg2)),
            CONTROL_CHANGE     => Some(MidiMsg::ControlChangeNumValue(input.arg1, input.arg2)),
            _ => None
        }
    }

    pub fn from_byte_array(input: &[u8]) -> Option<Self> {
        Self::from_bytes(MidiBytes::from(input))
    }

    /// A pitch bend message has 14 bits of resolution. The 7 low-order bits come first,
    /// followed by the 7 high-order bits.
    pub fn pitch_bend_value(lsb: u8, msb: u8) -> u16 {
        lsb as u16 | ((msb as u16) << 7)
    }
}