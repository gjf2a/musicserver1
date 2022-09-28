// Where to lookup vendor and product ID:
//
// https://the-sz.com/products/usbid/?v=
//
// Downloadable list:
// http://www.linux-usb.org/usb.ids

use std::time::Duration;
use musicserver1::midi::{MidiBytes, MidiMsg};
use musicserver1::usb_midi::print_device_list;

const XFER_INTERFACE: u8 = 1;
const MAX_INPUT_BYTES: usize = 128;
const INPUT_ENDPOINT: u8 = 129;
const INPUT_TIMEOUT_MS: u64 = 1;

fn main() -> std::io::Result<()> {
    for device in rusb::devices().unwrap().iter() {
        let device_desc = device.device_descriptor().unwrap();

        println!("Bus {:03} Device {:03} ID {:04x}:{:04x}",
                 device.bus_number(),
                 device.address(),
                 device_desc.vendor_id(),
                 device_desc.product_id());
        println!("# configurations: {}", device_desc.num_configurations());
        let config = device.active_config_descriptor().unwrap();
        println!("# interfaces: {}", config.num_interfaces());
        for interface in config.interfaces() {
            println!("Interface #{}", interface.number());
            for interface_desc in interface.descriptors() {
                println!("# endpoints: {}", interface_desc.num_endpoints());
                for endpoint_desc in interface_desc.endpoint_descriptors() {
                    println!("Endpoint #{}", endpoint_desc.number());
                    println!("Endpoint Address:       {}", endpoint_desc.address());
                    println!("Endpoint Direction:     {:?}", endpoint_desc.direction());
                    println!("Endpoint Sync:          {:?}", endpoint_desc.sync_type());
                    println!("Endpoint Transfer Type: {:?}", endpoint_desc.transfer_type());
                    println!("Endpoint Usage Type:    {:?}", endpoint_desc.usage_type())
                }
            }
        }
        println!();
    }

    let choice = print_device_list()?;
    println!("You chose {:?}", choice);
    Ok(())
}