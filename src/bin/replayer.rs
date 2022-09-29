// Right keyboard on Mac: Bus 020 Device 006 ID 1c75:0289
// Left keyboard on Mac:  Bus 020 Device 007 ID 1c75:0289
//
// Keyboard info:
// # configurations: 1
// # interfaces: 2
// Interface #0
// # endpoints: 0
// Interface #1
// # endpoints: 2
// Endpoint #1
// Endpoint Address:       1
// Endpoint Direction:     Out
// Endpoint Sync:          NoSync
// Endpoint Transfer Type: Bulk
// Endpoint Usage Type:    Data
// Endpoint #1
// Endpoint Address:       129
// Endpoint Direction:     In
// Endpoint Sync:          NoSync
// Endpoint Transfer Type: Bulk
// Endpoint Usage Type:    Data

// Sonuus on Windows: Bus 002 Device 011 ID 231c:0001
// # configurations: 1
// # interfaces: 4
// Interface #0
// # endpoints: 1
// Endpoint #4
// Endpoint Address:       132
// Endpoint Direction:     In
// Endpoint Sync:          NoSync
// Endpoint Transfer Type: Interrupt
// Endpoint Usage Type:    Data
// Interface #1
// # endpoints: 0
// # endpoints: 1
// Endpoint #1
// Endpoint Address:       129
// Endpoint Direction:     In
// Endpoint Sync:          Asynchronous
// Endpoint Transfer Type: Isochronous
// Endpoint Usage Type:    Data
// Interface #2
// # endpoints: 2
// Endpoint #2
// Endpoint Address:       2
// Endpoint Direction:     Out
// Endpoint Sync:          NoSync
// Endpoint Transfer Type: Bulk
// Endpoint Usage Type:    Data
// Endpoint #2
// Endpoint Address:       130
// Endpoint Direction:     In
// Endpoint Sync:          NoSync
// Endpoint Transfer Type: Bulk
// Endpoint Usage Type:    Data
// Interface #3
// # endpoints: 1
// Endpoint #3
// Endpoint Address:       131
// Endpoint Direction:     In
// Endpoint Sync:          NoSync
// Endpoint Transfer Type: Interrupt
// Endpoint Usage Type:    Data

use nannou::prelude::*;
use nannou_audio as audio;
use nannou_audio::Buffer;
use std::f64::consts::PI;

use std::time::Duration;
use musicserver1::midi::{MidiBytes, MidiMsg};
use musicserver1::usb_midi::user_select_device;

const MAX_INPUT_BYTES: usize = 128;
const INPUT_TIMEOUT_MS: u64 = 1;

fn main() -> std::io::Result<()> {
    let (device, interface, endpoint_addr) = user_select_device()?;
    let mut handle = device.open().unwrap();
    handle.claim_interface(interface).unwrap();
    let mut buf = [0; MAX_INPUT_BYTES];
    let input_wait = Duration::from_millis(INPUT_TIMEOUT_MS);
    loop {
        match handle.read_bulk(endpoint_addr, &mut buf, input_wait) {
            Ok(_) => {
                let bytes = MidiBytes::from(&buf);
                let msg = MidiMsg::from_bytes(bytes);
                println!("{:?} ({:?})", msg, bytes);
            }
            Err(e) => match e {
                rusb::Error::Timeout => {}
                _ => {
                    println!("Error: {:?}", e);
                    break;
                }
            }
        }
    }
    Ok(())
}