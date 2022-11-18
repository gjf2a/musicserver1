use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{Device, Sample, SampleFormat, StreamConfig};
use fundsp::hacker::{constant, midi_hz, triangle};
use fundsp::prelude::AudioUnit64;

fn main() {
    let host = cpal::default_host();
    let device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config = device.default_output_config().unwrap();
    match config.sample_format() {
        SampleFormat::F32 => run_synth::<f32>(device, config.into()),
        SampleFormat::I16 => run_synth::<i16>(device, config.into()),
        SampleFormat::U16 => run_synth::<u16>(device, config.into()),
    }
}

fn run_synth<T: Sample>(device: Device, config: StreamConfig) {
    let sample_rate = config.sample_rate.0 as f64;
    let mut sound = (constant(midi_hz(60.0)) >> triangle())
        + (constant(midi_hz(64.0)) >> triangle())
        + (constant(midi_hz(67.0)) >> triangle())
        + (constant(midi_hz(71.0)) >> triangle())
        + (constant(midi_hz(72.0)) >> triangle())
        + (constant(midi_hz(74.0)) >> triangle())
        + (constant(midi_hz(78.0)) >> triangle())
        + (constant(midi_hz(81.0)) >> triangle());
    sound.reset(Some(sample_rate));
    let mut next_value = move || sound.get_stereo();
    let channels = config.channels as usize;
    let err_fn = |err| eprintln!("an error occurred on stream: {err}");
    let stream = device
        .build_output_stream(
            &config,
            move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
                write_data(data, channels, &mut next_value)
            },
            err_fn,
        )
        .unwrap();

    stream.play().unwrap();
    loop {}
}

fn write_data<T: Sample>(
    output: &mut [T],
    channels: usize,
    next_sample: &mut dyn FnMut() -> (f64, f64),
) {
    for frame in output.chunks_mut(channels) {
        let sample = next_sample();
        let left: T = Sample::from::<f32>(&(sample.0 as f32));
        let right: T = Sample::from::<f32>(&(sample.1 as f32));

        for (channel, sample) in frame.iter_mut().enumerate() {
            *sample = if channel & 1 == 0 { left } else { right };
        }
    }
}
