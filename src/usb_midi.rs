use std::collections::BTreeMap;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Write};
use std::str::SplitWhitespace;
use rusb::{Device, GlobalContext};

pub fn print_device_list() -> io::Result<(Device<GlobalContext>)> {
    let table = VendorProductTable::new().unwrap();
    let mut devices = vec![];
    for (i, device) in rusb::devices().unwrap().iter().enumerate() {
        let device_desc = device.device_descriptor().unwrap();
        let vid = device_desc.vendor_id();
        let pid = device_desc.product_id();
        let vname = table.get_vendor_name(vid).unwrap_or("Unknown vendor".to_owned());
        let pname = table.get_product_name(vid, pid).unwrap_or("Unknown product".to_owned());
        println!("{}) {} {}", i+1, vname, pname);
        devices.push(device);
    }
    let stdin = io::stdin();
    loop {
        print!("Enter number of preferred device: ");
        io::stdout().flush()?;
        let mut line = String::new();
        stdin.read_line(&mut line)?;
        if let Ok(num) = line.trim().parse::<usize>() {
            if num >= 1 && num <= devices.len() {
                return Ok(devices[num - 1].clone());
            }
        }
        println!("'{}' was not a valid input. Try again.", line);
    }
}

pub fn hex2u16(input: &str) -> Option<u16> {
    let input = input.to_lowercase();
    if input.trim().len() == 4 {
        let mut result = 0;
        for value in input.chars().map(|c| digit2value(c)) {
            if let Some(value) = value {
                result <<= 4;
                result |= value;
            } else {
                return None;
            }
        }
        Some(result)
    } else {
        None
    }
}

pub fn digit2value(digit: char) -> Option<u16> {
    if ('0'..='9').contains(&digit) {
        Some(digit as u16 - '0' as u16)
    } else if ('a'..='f').contains(&digit) {
        Some(digit as u16 - 'a' as u16 + 10)
    } else {
        None
    }
}

pub struct VendorProductTable {
    vendor2products: BTreeMap<u16,Vec<u16>>,
    vendor_names: BTreeMap<u16, String>,
    product_names: BTreeMap<(u16,u16), String>
}

impl VendorProductTable {
    pub fn new() -> std::io::Result<Self> {
        let mut result = VendorProductTable {vendor_names: BTreeMap::new(), vendor2products: BTreeMap::new(), product_names: BTreeMap::new()};
        result.add_file_entries("usb.ids.txt")?;
        result.add_file_entries("usb_supplement.ids.txt")?;
        Ok(result)
    }

    pub fn get_vendor_name(&self, vendor: u16) -> Option<String> {
        self.vendor_names.get(&vendor).map(|v| v.clone())
    }

    pub fn get_product_name(&self, vendor: u16, product: u16) -> Option<String> {
        self.product_names.get(&(vendor, product)).map(|p| p.clone())
    }

    fn add_file_entries(&mut self, filename: &str) -> std::io::Result<()> {
        let file = File::open(filename)?;
        let reader = BufReader::new(file);
        let mut current_vendor = None;
        for line in reader.lines() {
            let line = line?;
            let line = line.trim_end();
            if line == "# List of known device classes, subclasses and protocols" {
                break;
            }
            if line.len() > 0 {
                match line.chars().next().unwrap() {
                    '0'..='9' | 'a'..='f' => {
                        let (vendor_id, vendor_name) = id_and_name_from(line.split_whitespace());
                        self.vendor_names.insert(vendor_id, vendor_name);
                        if !self.vendor2products.contains_key(&vendor_id) {
                            self.vendor2products.insert(vendor_id, vec![]);
                        }
                        current_vendor = Some(vendor_id);
                    }
                    '\t' => {
                        let (product_id, product_name) = id_and_name_from(line.trim().split_whitespace());
                        self.vendor2products.get_mut(&current_vendor.unwrap()).unwrap().push(product_id);
                        self.product_names.insert((current_vendor.unwrap(), product_id), product_name);
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }
}

fn id_and_name_from(mut parts: SplitWhitespace) -> (u16, String) {
    let id_num = hex2u16(parts.next().unwrap()).unwrap();
    let name = parts.map(|s| s.to_owned() + " ").collect::<String>();
    (id_num, name.trim().to_owned())
}

#[cfg(test)]
mod tests {
    use crate::usb_midi::{digit2value, hex2u16, VendorProductTable};

    #[test]
    fn test_hex() {
        for (hex, value) in [
            ("1234", 4660), ("0000", 0), ("FFFF", 65535), ("ABCD", 43981), ("abcd", 43981),
            ("e000", 57344), ("5678", 22136), ("89ab", 35243)
        ] {
            assert_eq!(hex2u16(hex).unwrap(), value);
        }
    }

    #[test]
    fn test_digit() {
        for (i, digit) in ('0'..='9').enumerate() {
            assert_eq!(i as u16, digit2value(digit).unwrap());
        }
        for (i, digit) in ('a'..='f').enumerate() {
            assert_eq!((i + 10) as u16, digit2value(digit).unwrap());
        }
    }

    #[test]
    fn test_table() {
        let table = VendorProductTable::new().unwrap();
        for (vid, pid, vname, pname) in [
            (0x1c75, 0x0289, "Arturia", "Mark II"),
            (0x231c, 0x0001, "Sonuus Limited", "i2M Musicport"),
            (0x1c75, 0x0288, "Arturia", "KeyStep"),
            (0x2367, 0x0002, "Teenage Engineering", "OP-1 Portable synthesizer"),
            (0x2367, 0x000c, "Teenage Engineering", "OP-Z Portable synthesizer"),
            (0x0499, 0x1054, "Yamaha Corp.", "S90XS Keyboard/Music Synthesizer"),
            (0x0582, 0x001d, "Roland Corp.", "V-SYNTH"),
            (0x0582, 0x001e, "Roland Corp.", "V-SYNTH"),
            (0x0582, 0x002d, "Roland Corp.", "XV-2020 Synthesizer"),
            (0x0582, 0x002e, "Roland Corp.", "XV-2020 Synthesizer")
        ] {
            println!("{} {}", vname, pname);
            let vidname = table.get_vendor_name(vid).unwrap();
            let pidname = table.get_product_name(vid, pid).unwrap();
            assert_eq!(vidname, vname);
            assert_eq!(pidname, pname);
        }
    }
}