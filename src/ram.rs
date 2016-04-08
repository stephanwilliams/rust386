use alloc::heap;

use clock::{ Clocked };
use bus::{ BusState, BusLine, Signal };

pub struct MemoryController {
    memory: Vec<u32>
}

impl MemoryController {
    pub fn new(size: usize) -> MemoryController {
        unsafe {
            let mem = heap::allocate(size >> 2, 4);
        }
        assert!(size & 0x3 == 0, "memory size must be multiple of 4");

        MemoryController {
            memory: vec![0; 1]
        }
    }
}

impl Clocked<BusState, BusState> for MemoryController {
    fn rising_edge(&mut self, state: BusState) -> BusState {
        if state.read(BusLine::ADS) == Signal::Low
            && state.read(BusLine::D_C) == Signal::High
            && state.read(BusLine::M_IO) == Signal::High {
                debug!("received memory request {:x}", state.read_address());
            }

        BusState::new()
    }

    fn falling_edge(&mut self, state: BusState) -> BusState {
        BusState::new()
    }
}
