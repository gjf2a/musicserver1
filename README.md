# musicserver1

This repository represents a research project I am undertaking, to give musicians practice improvising in a 
[call-and-response](https://jamplay.com/weekend-warrior/w/call-and-response-like-a-blues-legend) format. My purpose
in undertaking this project is to create a resource that enables musicians to improve their improvisational skills.

There are several executables in the [`\bin`](https://github.com/gjf2a/musicserver1/tree/master/src/bin) directory. 
The one to run is [`replayer_gui.rs`](https://github.com/gjf2a/musicserver1/blob/master/src/bin/replayer_gui.rs). 

When you start the program, it will automatically identify all MIDI devices attached to your computer. If there is 
more than one, it will ask you to select one to use. When it starts, it will simply let you play your instrument,
without any response. You can select one of the response algorithms to produce a response after you stop playing
for an amount of time specified by the delay slider. (Default is 1.5 seconds.) At that point, it will compose and
perform a response.

This program is very much a work in progress. Contributions are welcome - see the 
[Issues page](https://github.com/gjf2a/musicserver1/issues) for ideas. 

At some point, I plan to change the name - this originated as a TCP server for use with Sonic Pi, until I decided
I preferred a standalone program. 

## Compiling

To compile and execute the program, run the following at the command line:

```
cargo run --bin replayer_gui --release
```

The `--release` is very important - sound quality often suffers if compiler optimizations aren't enabled.

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
