use std::cell::{ RefCell };
use std::ops::{ Range };

use clock::{ Clocked };
use bus::{ BusState, BusLine, Signal };

pub trait IoDevice {
    const PORT_COUNT: u16;

    fn read_u8(&mut self, port: u16) -> u8;

    fn read_u16(&mut self, port: u16) -> u16 {
        assert!(port + 1 < Self::PORT_COUNT, "invalid port");
        assert!(port & 0x1 == 0, "port must be 2-byte aligned");

        let high = self.read_u8(port) as u16;
        let low = self.read_u8(port + 1) as u16;

        (high << 8) | low
    }

    fn read_u32(&mut self, port: u16) -> u32 {
        assert!(port + 3 < Self::PORT_COUNT, "invalid port");
        assert!(port & 0x3 == 0, "port must be 4-byte aligned");

        let hh = self.read_u8(port + 3) as u32;
        let hl = self.read_u8(port + 2) as u32;
        let lh = self.read_u8(port + 1) as u32;
        let ll = self.read_u8(port) as u32;

        (hh << 24) | (hl << 16) | (lh << 8) | ll
    }

    fn write_u8(&mut self, port: u16, data: u8);

    fn write_u16(&mut self, port: u16, data: u16) {
        assert!(port + 1 < Self::PORT_COUNT, "invalid port");
        assert!(port & 0x1 == 0, "port must be 2-byte aligned");
        
        self.write_u8(port, data as u8);
        self.write_u8(port + 1, (data >> 8) as u8);
    }

    fn write_u32(&mut self, port: u16, data: u32) {
        assert!(port + 3 < Self::PORT_COUNT, "invalid port");
        assert!(port & 0x3 == 0, "port must be 4-byte aligned");

        self.write_u8(port, data as u8);
        self.write_u8(port + 1, (data >> 8) as u8);
        self.write_u8(port + 2, (data >> 16) as u8);
        self.write_u8(port + 3, (data >> 24) as u8);
    }
}

pub struct IoBus<'a> {
    iodevs: Vec<(Range<u32>, RefCell<Box<IoDevice + 'a>>)>,

    bus_is_valid: bool,
    bus_is_write: bool, // vs write
    bus_addr: u16,
    bus_data: u32,
    bus_data_size: usize
}

impl<'a> IoBus<'a> {
    pub fn new() -> IoBus<'a> {
        IoBus {
            iodevs: vec![],

            bus_is_valid: false,
            bus_is_write: false,
            bus_addr: 0,
            bus_data: 0,
            bus_data_size: 0
        }
    }

    pub fn map_io<T>(&mut self, iodev: T, range: Range<u32>) where T: IoDevice + 'a {
        self.iodevs.push((range, RefCell::new(Box::new(iodev))));
    }
}

impl<'a> Clocked<BusState, BusState> for IoBus<'a> {
    fn rising_edge(&mut self, state: BusState) -> BusState {
        trace!("io bus rising");

        BusState::new()
    }

    fn falling_edge(&mut self, state: BusState) -> BusState {
        trace!("io bus falling");
        let mut new_state = BusState::new();

        if state.read(BusLine::ADS) == Signal::Low
            && state.read(BusLine::D_C) == Signal::High
            && state.read(BusLine::M_IO) == Signal::Low {
            
            self.bus_is_valid = true;
            self.bus_is_write = state.read(BusLine::W_R) == Signal::High;
            self.bus_addr = state.read_address() as u16;
            self.bus_data = state.read_data();
            self.bus_data_size = state.read_size();

            trace!("received io request {:x}", state.read_address());
        }

        if self.bus_is_valid {
            let addr = state.read_address() as u16;

            for &(ref range, ref iodev) in self.iodevs.iter() {
                if range.start <= (addr as u32) && (addr as u32) < range.end {
                    let port = addr - (range.start as u16);
                    trace!("FOUND IO DEVICE PORT {}", port);
                    match self.bus_is_write {
                        true => {
                            let data = self.bus_data;
                            match self.bus_data_size {
                                1 => iodev.borrow_mut().write_u8(port, data as u8),
                                2 => iodev.borrow_mut().write_u16(port, data as u16),
                                4 => iodev.borrow_mut().write_u32(port, data),
                                _ => panic!("invalid size write")
                            }
                        },
                        false => {
                            new_state.assert_data(match self.bus_data_size {
                                1 => iodev.borrow_mut().read_u8(port) as u32,
                                2 => iodev.borrow_mut().read_u16(port) as u32,
                                4 => iodev.borrow_mut().read_u32(port),
                                _ => panic!("invalid size read")
                            }, self.bus_data_size, (addr % 4) as usize);
                        }
                    }
                }
            }

            self.bus_is_valid = false;
            trace!("io bus action");
            new_state.assert(BusLine::READY, Signal::Low);
        }

        new_state
    }
}
