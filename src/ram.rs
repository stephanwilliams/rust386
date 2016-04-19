use alloc::heap;

use clock::{ Clocked };
use bus::{ BusState, BusLine, Signal };

pub struct MemoryController {
    memory: *mut u32
}

impl MemoryController {
    pub fn new(size: usize) -> MemoryController {
        assert!(size & 0x3 == 0, "memory size must be multiple of 4");

        MemoryController {
            memory: unsafe { heap::allocate(size, 4096) as *mut u32 }
        }
    }
}

impl Clocked<BusState, BusState> for MemoryController {
    fn rising_edge(&mut self, state: BusState) -> BusState {
        BusState::new()
    }

    fn falling_edge(&mut self, state: BusState) -> BusState {
        let mut new_state = BusState::new();

        if state.read(BusLine::ADS) == Signal::Low
            && state.read(BusLine::D_C) == Signal::High
            && state.read(BusLine::M_IO) == Signal::High {
                debug!("received memory request {:x}", state.read_address());

                let addr = state.read_address();
                let addr4 = (addr >> 2) as isize;
                let size = state.read_size();
                assert!(addr & 0x3 == 0);
                assert!(size == 4);

                match state.read(BusLine::W_R) {
                    Signal::High => {
                        let data = state.read_data();
                        trace!("MEMWR {:08x} {:08x}", data, addr);
                        unsafe { *self.memory.offset(addr4) = data };
                    },
                    Signal::Low => {
                        new_state.assert_data(unsafe {
                            *self.memory.offset(addr4)}, 4, 0);
                    },
                    _ => panic!("mem w/r undefined")
                }

                new_state.assert(BusLine::READY, Signal::Low);
            }

        new_state
    }
}
