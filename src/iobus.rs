use std::cell::{ RefCell };
use std::ops::{ Range };

use clock::{ Clocked, ClockState };
use bus::{ BusState, BusLine, Signal };

pub trait IoDevice {
    fn port_count(&self) -> u16;

    fn read_u8(&self, port: u16) -> u8;

    fn read_u16(&self, port: u16) -> u16 {
        assert!(port + 1 < self.port_count(), "invalid port");
        assert!(port & 0x1 == 0, "port must be 2-byte aligned");

        let high = self.read_u8(port) as u16;
        let low = self.read_u8(port + 1) as u16;

        (high << 8) | low
    }

    fn read_u32(&self, port: u16) -> u32 {
        assert!(port + 3 < self.port_count(), "invalid port");
        assert!(port & 0x3 == 0, "port must be 4-byte aligned");

        let hh = self.read_u8(port) as u32;
        let hl = self.read_u8(port + 1) as u32;
        let lh = self.read_u8(port + 2) as u32;
        let ll = self.read_u8(port + 3) as u32;

        (hh << 24) | (hl << 16) | (lh << 8) | ll
    }

    fn write_u8(&self, port: u16, data: u8);

    fn write_u16(&self, port: u16, data: u16) {
        assert!(port + 1 < self.port_count(), "invalid port");
        assert!(port & 0x1 == 0, "port must be 2-byte aligned");
        
        self.write_u8(port, data as u8);
        self.write_u8(port + 1, (data >> 8) as u8);
    }

    fn read_u32(&self, port: u16, data: u32) {
        assert!(port + 3 < self.port_count(), "invalid port");
        assert!(port & 0x3 == 0, "port must be 4-byte aligned");

        self.write_u8(port, data as u8);
        self.write_u8(port + 1, (data >> 8) as u8);
        self.write_u8(port + 2, (data >> 16) as u8);
        self.write_u8(port + 3, (data >> 24) as u8);
    }
}

pub struct IoBus<'a> {
    iodevs: Vec<(Range<u32>, RefCell<Box<IoDevice + 'a>>)>
}

impl<'a> IoBus<'a> {
    pub fn new() -> IoBus<'a> {
        IoBus {
            iodevs: vec![]
        }
    }

    pub fn map_io<T>(&mut self, iodev: T, range: Range<u32>) where T: IoDevice + 'a {
        self.iodevs.push((range, RefCell::new(Box::new(iodev))));
    }
}

impl<'a> Clocked<BusState, BusState> for IoBus<'a> {
    fn rising_edge(&mut self, state: BusState) -> BusState {
        if state.read(BusLine::ADS) == Signal::Low
            && state.read(BusLine::D_C) == Signal::High
            && state.read(BusLine::M_IO) == Signal::Low {

            let addr = state.read_address();

            for &(ref range, ref iodev) in self.iodevs.iter() {
                if range.start <= (addr as u32) && (addr as u32) < range.end {
                    match BusLine::W_R {
                        Signal::High => {

                        }, Signal::Low => {

                        }
                    }
                }
            }

            debug!("received io request {:x}", state.read_address());
        }

        state
    }

    fn falling_edge(&mut self, state: BusState) -> BusState {
        state
    }
}
