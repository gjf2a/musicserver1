// Right keyboard on Mac: Bus 020 Device 006 ID 1c75:0289
// Left keyboard on Mac:  Bus 020 Device 007 ID 1c75:0289

use std::time::Duration;

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

        let mut handle = device.open().unwrap();
        handle.claim_interface(XFER_INTERFACE).unwrap();
        let mut buf = [0; MAX_INPUT_BYTES];
        let input_wait = Duration::from_millis(INPUT_TIMEOUT_MS);
        loop {
            match handle.read_bulk(INPUT_ENDPOINT, &mut buf, input_wait) {
                Ok(n) => {
                    println!("Read {} bytes: {:?}", n, &buf[0..n]);
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
    }
    Ok(())
}