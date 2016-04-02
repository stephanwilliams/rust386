use clock::{ Clocked, Clock, ClockState };
use std::cell::Cell;
use std::cell::RefCell;

#[derive(Clone, Copy, PartialEq, Eq)]
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
    A0, A1, A2, A3, A4, A5, A6, A7,
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
        assert!(line != BusLine::A0 && line != BusLine::A1, "A0 and A1 can only be read");
        self.lines[line as usize] = signal;
    }

    pub fn read(&self, line: BusLine) -> Signal {
        self.lines[line as usize]
    }

    pub fn assert_address(&mut self, addr: u32) {
        let off = BusLine::A0 as usize;
        for i in 2..32 {
            self.lines[off + i] = if (addr >> i) & 1 == 1 { Signal::High } else { Signal::Low };
        }
    }

    pub fn read_address(&self) -> u32 {
        let off = BusLine::A0 as usize;
        let mut addr = 0;
        for i in 2..32 {
            match self.lines[off + i] {
                Signal::High => { addr |= 1 << i; }
                _ => { }
            }
        }

        addr
    }

    pub fn assert_data(&mut self, data: u32) {
        let off = BusLine::D0 as usize;
        for i in 0..32 {
            self.lines[off + i] = if (data >> i) & 1 == 1 { Signal::High } else { Signal::Low };
        }
    }

    pub fn read_data(&self) -> u32 {
        let off = BusLine::D0 as usize;
        let mut data = 0;
        for b in 0..4 {
            if self.lines[BusLine::BE0 as usize + b] == Signal::High {
                let byte_off = b * 8;
                for i in byte_off..byte_off + 8 {
                    match self.lines[byte_off + off + i] {
                        Signal::High => { data |= 1 << (i + byte_off); }
                        _ => { }
                    }
                }
            }
        }

        data
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
        let mut s = BusState::unit();

        for clocked in self.clockeds.iter_mut() {
            let cmut = &mut *clocked.borrow_mut();
            s = s.coalesce(cmut.rising_edge(self.bus_state.clone()));
        }

        self.bus_state = s;
    }

    fn falling_edge(&mut self, state: ()) {
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