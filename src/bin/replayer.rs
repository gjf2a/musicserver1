use std::sync::Arc;
use anyhow::bail;
use midir::{MidiInput, Ignore, MidiInputPort};
use musicserver1::input_cmd;
use midi_msg::MidiMsg;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use fundsp::hacker::*;
use crossbeam_queue::SegQueue;


fn main() -> anyhow::Result<()> {
    let mut midi_in = MidiInput::new("midir reading input")?;
    midi_in.ignore(Ignore::None);

    let in_ports = midi_in.ports();
    let in_port = match in_ports.len() {
        0 => bail!("no input port found"),
        1 => {
            println!("Choosing the only available input port: {}", midi_in.port_name(&in_ports[0]).unwrap());
            &in_ports[0]
        },
        _ => {
            println!("\nAvailable input ports:");
            for (i, p) in in_ports.iter().enumerate() {
                println!("{}: {}", i, midi_in.port_name(p).unwrap());
            }
            let input = input_cmd("Please select input port: ")?;
            match in_ports.get(input.trim().parse::<usize>()?) {
                None => bail!("invalid input port selected"),
                Some(p) => p
            }
        }
    };

    let midi_queue = Arc::new(SegQueue::new());
    let host = cpal::default_host();

    let device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config = device.default_output_config().unwrap();

    match config.sample_format() {
        cpal::SampleFormat::F32 => run::<f32>(midi_queue.clone(), &device, &config.into()).unwrap(),
        cpal::SampleFormat::I16 => run::<i16>(midi_queue.clone(), &device, &config.into()).unwrap(),
        cpal::SampleFormat::U16 => run::<u16>(midi_queue.clone(), &device, &config.into()).unwrap(),
    }

    println!("\nOpening connection");
    let in_port_name = midi_in.port_name(in_port)?;

    // _conn_in needs to be a named parameter, because it needs to be kept alive until the end of the scope
    let _conn_in = midi_in.connect(in_port, "midir-read-input", move |stamp, message, _| {
        let (msg, len) = MidiMsg::from_midi(&message).unwrap();
        println!("msg: {:?} len: {}", msg, len);
        midi_queue.push(msg);
    }, ())?;

    println!("Connection open, reading input from '{}'", in_port_name);

    let _ = input_cmd("(press enter to exit)...")?;
    println!("Closing connection");
    Ok(())
}

fn run<T>(incoming: Arc<SegQueue<MidiMsg>>, device: &cpal::Device, config: &cpal::StreamConfig) -> anyhow::Result<()>
    where
        T: cpal::Sample,
{
    let sample_rate = config.sample_rate.0 as f64;
    let channels = config.channels as usize;

    std::thread::spawn(move || {
        loop {
            if let Some(msg) = incoming.pop() {
                println!("Received {:?}", msg);
            }
        }
    });

    Ok(())
}

fn write_data<T>(output: &mut [T], channels: usize, next_sample: &mut dyn FnMut() -> (f64, f64))
    where
        T: cpal::Sample,
{
    for frame in output.chunks_mut(channels) {
        let sample = next_sample();
        let left: T = cpal::Sample::from::<f32>(&(sample.0 as f32));
        let right: T = cpal::Sample::from::<f32>(&(sample.1 as f32));

        for (channel, sample) in frame.iter_mut().enumerate() {
            if channel & 1 == 0 {
                *sample = left;
            } else {
                *sample = right;
            }
        }
    }
}