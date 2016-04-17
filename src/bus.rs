use clock::{ Clocked, Clock, ClockState };
use std::cell::RefCell;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Signal {
    Undefined,
    High,
    Low,
}

#[derive(PartialEq, Eq)]
pub enum DataSize {
    Bits32 = 0xFFFFFFFF,
    Bits24 = 0x00FFFFFF,
    Bits16 = 0x0000FFFF,
    Bits8  = 0x000000FF,
}

#[derive(PartialEq, Eq)]
pub enum BusLine {
    D0, D1, D2, D3, D4, D5, D6, D7,
    D8, D9, D10, D11, D12, D13, D14, D15,
    D16, D17, D18, D19, D20, D21, D22, D23,
    D24, D25, D26, D27, D28, D29, D30, D31,
    BE0,
    BE1,
    BE2,
    BE3,
    A2, A3, A4, A5, A6, A7,
    A8, A9, A10, A11, A12, A13, A14, A15,
    A16, A17, A18, A19, A20, A21, A22, A23,
    A24, A25, A26, A27, A28, A29, A30, A31,
    W_R,
    D_C,
    M_IO,
    LOCK,
    ADS,
    NA, // always high
    BS16, // always high
    READY,
    HOLD, // unimplemented
    HLDA, // unimplemented
    PEREQ, // unimplemented
    BUSY, // unimplemented
    ERROR, // unimplemented
    INTR, // unimplemented
    NMI, // unimplemented
    RESET,
}

pub struct BusState {
    // data bus

    // data: u32,      //   / S

    // byte enables

    // must be contiguous
    // If only high 2 bytes are being transfered, high 2 bytes are duplicated
    // to low 2 bytes

    // be0: Signal,    // # O -
    // be1: Signal,    // # O -
    // be2: Signal,    // # O -
    // be3: Signal,    // # O -

    // address bus

    // 00000000h-FFFFFFFFh memory
    // 00000000h-FFFFFFFFh I/O
    // 800000F8h-800000FFh coprocessor
    // address: u32,   //   O -

    // bus control

    // indicates wr#, dc#, mio#, address valid
    // ads: Signal,    // # O - address status
    // na: Signal,     // # I S next address request
    // bs16: Signal,   // # I S bus size 16
    // indicates current bus cycle is complete
    // active bytes indicated by be_# and bs16# are accepted/provided
    //
    // ready: Signal,  // # I S transfer acknowledge

    // bus cycle definition

    // e.g. M/IO# high -> M, low -> IO
    
    // M/IO# | D/C# | W/R# | Locked | Bus Cycle type
    // Low   | Low  | Low  | Yes    | INTERRUPT ACKNOWLEDGE
    // Low   | Low  | High | -      | n/a
    // Low   | High | Low  | No     | I/O DATA READ
    // Low   | High | High | No     | I/O DATA WRITE
    // High  | Low  | Low  | No     | MEMORY CODE READ
    // High  | Low  | High | No     | HALT (BE2#), SHUTDOWN (BE0#)
    // High  | High | Low  | Some   | MEMORY DATA READ
    // High  | High | High | Some   | MEMORY DATA WRITE

    // wr: Signal,     // # O - write-read indication
    // dc: Signal,     // # O - data-control indication
    // mio: Signal,    // # O - memory-i/o indication
    // valid as first locked bus cycle begins
    // negated when the ready# input terminates the last bus cycle which was locked
    // lock: Signal,   // # O - bus lock indication

    // bus arbitration

    // hold: Signal,   //   I S bus hold request
    // hlda: Signal,   //   O - bus hold acknowledge

    // interrupts

    // intr: Signal,   //   I A
    // nmi: Signal,    //   I A
    // reset: Signal,  //   I S

    // coprocessor signalling

    // pereq: Signal,  //   I A
    // busy: Signal,   // # I A
    // error: Signal,  // # I A
    
    lines: [Signal; 84]
}

impl Clone for BusState {
    fn clone(&self) -> BusState {
        BusState {
            lines: self.lines
        }
    }
}

impl BusState {
    pub fn new() -> BusState {
        BusState {
            lines: [Signal::Undefined; 84]
        }
    }

    pub fn assert(&mut self, line: BusLine, signal: Signal) {
        self.lines[line as usize] = signal;
    }

    pub fn read(&self, line: BusLine) -> Signal {
        self.lines[line as usize]
    }

    pub fn assert_address(&mut self, addr: u32, size: usize) {
        let off = (addr & 0b11) as usize;
        trace!("bus assert addr {:08x} size {:08x} off {}", addr, size, off);
        assert!((addr % 4) + (size as u32) <= 4, "assert invalid address for size on bus");

        let a2_off = BusLine::A2 as usize - 2;
        for i in 2..32 {
            self.lines[a2_off + i] = if (addr >> i) & 1 == 1 { Signal::High } else { Signal::Low };
        }

        let be_off = BusLine::BE0 as usize;
        for i in 0..4 {
            self.lines[be_off + i] =
                if i >= off && i < off + size { Signal::Low } else { Signal::High };
        }
    }

    pub fn assert_address_and_data(&mut self, addr: u32, data: u32, size: usize) {
        let off = (addr & 0x3) as usize;
        assert!((addr % 4) + (size as u32) <= 4, "assert invalid bus address for data size");
        self.assert_address(addr, size);
        self.assert_data(data, size, off);
    }

    // pub fn assert_address(&mut self, addr: u32) {
    //     assert!(addr & 0x3 == 0, "assert unaligned address on bus");
    //     let off = BusLine::A2 as usize - 2;
    //     for i in 2..32 {
    //         self.lines[off + i] = if (addr >> i) & 1 == 1 { Signal::High } else { Signal::Low };
    //     }
    // }

    pub fn read_address(&self) -> u32 {
        let off = BusLine::A2 as usize - 2;
        let mut addr = 0;
        for i in 2..32 {
            match self.lines[off + i] {
                Signal::High => { addr |= 1 << i; }
                _ => { }
            }
        }

        let be_off = BusLine::BE0 as usize;
        let mut ind = 0;
        while ind < 3 {
            if self.lines[be_off + ind] == Signal::High {
                ind += 1;
            } else {
                break;
            }
        }

        addr + (ind as u32)
    }

    pub fn assert_data(&mut self, data: u32, size: usize, off: usize) {
        assert!(size == 1 || size == 2 || size == 3 || size == 4, "assert invalid data size on bus");
        assert!(off + size <= 4, "assert invalid offset on bus");

        // let be_off = BusLine::BE0 as usize;
        let d_off = BusLine::D0 as usize;

        for b in (0..4).rev() {
            // if off <= b && b < off + size {
            //     self.lines[be_off + b] = Signal::Low;
            // } else {
            //     self.lines[be_off + b] = Signal::High;
            // }

            for i in 0..8 {
                // let bit_off = b * 8 + i;
                // self.lines[d_off + bit_off] =
                //     if off >= 2 && b< 2 {
                //         self.lines[d_off + bit_off + 16];
                //     } else if off <= b && b < off + size {
                //         if (data >> )
                //     }

                self.lines[d_off + b * 8 + i] =
                    if off >= 2 && b < 2 {
                        self.lines[d_off + (b + 2) * 8 + i]
                    } else if off <= b && b < off + size {
                        if (data >> (8 * (b - off) + i)) & 1 == 1 {
                            Signal::High
                        } else {
                            Signal::Low
                        }
                    } else { Signal::Undefined }
            }
        }

        // for i in 0..32 {
        //     trace!("D{:-2} {:?}", i, self.lines[d_off + i]);
        // }
    }

    pub fn read_data(&self) -> u32 {
        let d_off = BusLine::D0 as usize;
        let mut data = 0;
        let mut off = 0;
        for b in 0..4 {
            if self.lines[BusLine::BE0 as usize + b] == Signal::Low {
                let byte_off = b * 8;
                for i in byte_off..byte_off + 8 {
                    match self.lines[d_off + i] {
                        Signal::High => { data |= 1 << (i - 8 * off); }
                        _ => { }
                    }
                }
            } else {
                off += 1;
            }
        }

        data
    }

    pub fn read_size(&self) -> usize {
        let be_off = BusLine::BE0 as usize;
        let mut off = 0;
        for i in 0..4 {
            if self.lines[be_off + i] == Signal::High {
                off += 1;
            } else {
                break;
            }
        }

        let mut bytes = 0;
        for i in off..4 {
            if self.lines[be_off + i] == Signal::Low {
                bytes += 1;
            } else {
                break;
            }
        }

        bytes as usize
    }
}

impl ClockState for BusState {
    fn coalesce(self, state: BusState) -> BusState {
        let mut bs = BusState::unit();
        for i in 0..bs.lines.len() {
            if self.lines[i] == state.lines[i] {
                bs.lines[i] = self.lines[i];
            } else if self.lines[i] == Signal::Undefined {
                bs.lines[i] = state.lines[i];
            } else if state.lines[i] == Signal::Undefined {
                bs.lines[i] = self.lines[i];
            } else {
                panic!("Invalid bus state");
            }
        }

        bs
    }

    fn unit() -> BusState {
        BusState::new()
    }
}

pub struct Bus<'a> {
    bus_state: BusState,
    clockeds: Vec<RefCell<Box<Clocked<BusState, BusState> + 'a>>>,
}

impl<'a> Bus<'a> {
    pub fn new() -> Bus<'a> {
        Bus {
            bus_state: BusState::new(),
            clockeds: vec![]
        }
    }
}

impl<'a> Clock<BusState, BusState> for Bus<'a> {
    fn add_clocked<T>(&mut self, clocked: T) where T: Clocked<BusState, BusState> + 'a {
        self.clockeds.push(RefCell::new(Box::new(clocked)));
    }
}

impl<'a> Clocked<(), ()> for Bus<'a> {
    fn rising_edge(&mut self, state: ()) {
        trace!("BUS RISING");
        let mut s = BusState::unit();

        for clocked in self.clockeds.iter_mut() {
            let cmut = &mut *clocked.borrow_mut();
            s = s.coalesce(cmut.rising_edge(self.bus_state.clone()));
        }

        self.bus_state = s;
    }

    fn falling_edge(&mut self, state: ()) {
        trace!("BUS FALLING");
        let mut s = BusState::unit();

        for clocked in self.clockeds.iter_mut() {
            let cmut = &mut *clocked.borrow_mut();
            s = s.coalesce(cmut.falling_edge(self.bus_state.clone()));
        }

        self.bus_state = s;
    }
}

// all 386 inputs are sampled at clk2 rising edges
// * some at every other rising edge
// * some when clk is high
// * some when clk is low

// Where to put clk2?
// clk2 is 2x freq of internal 386 clock
// use channels? or std::sync::Convar? or observer pattern

/*
 * "The 80386 can relinquish control of its local buses to allow mastership by
 *  other devides, such as direct memory access channels. When relinquished,
 *  HLDA is the only output pin driven by the 80386."
 */
