use numlib::traits::{ FromPrimitive };

use iobus::{ IoDevice };

pub const CRT_ROWS: u32 = 25;
pub const CRT_COLS: u32 = 80;

pub struct CGA {
    ram: [u8; 0x8000]
}

impl CGA {
    pub fn new() -> CGA {
        CGA {
            ram: [0; 0x8000]
        }
    }

    pub fn read_u8(&self, addr: u32) -> u8 {
        self.ram[addr as usize]
    }

    pub fn read_u16(&self, addr: u32) -> u16 {
        let high = self.read_u8(addr) as u16;
        let low = self.read_u8(addr + 1) as u16;

        (high << 8) | low
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        let hh = self.read_u8(addr) as u32;
        let hl = self.read_u8(addr + 1) as u32;
        let lh = self.read_u8(addr + 2) as u32;
        let ll = self.read_u8(addr + 3) as u32;

        (hh << 24) | (hl << 16) | (lh << 8) | ll
    }

    pub fn write_u8(&mut self, addr: u32, data: u8) {
        self.ram[addr as usize] = data;
    }

    fn write_u16(&mut self, addr: u32, data: u16) {
        self.write_u8(addr, data as u8);
        self.write_u8(addr + 1, (data >> 8) as u8);
    }

    pub fn write_u32(&mut self, addr: u32, data: u32) {
        self.write_u8(addr, data as u8);
        self.write_u8(addr + 1, (data >> 8) as u8);
        self.write_u8(addr + 2, (data >> 16) as u8);
        self.write_u8(addr + 3, (data >> 24) as u8);
    }
}

enum_primitive! {
    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
    enum CGARegister {
        HorizontalTotal,
        EndHorizontalDisplay,
        StartHorizontalBlanking,
        EndHorizontalBlanking,
        StartHorizontalRetrace,
        EndHorizontalRetrace,
        VerticalTotal,
        Overflow,
        PresetRowScan,
        MaximumScanLine,
        CursorStart,
        CursorEnd,
        StartAddressHigh,
        StartAddressLow,
        CursorLocationHigh,
        CursorLocationLow,
        VerticalRetraceStart,
        VerticalRetraceEnd,
        VerticalDisplayEnd,
        Offset,
        UnderlineLocation,
        StartVerticalBlanking,
        EndVerticalBlanking,
        CRTCModeControl,
        LineCompare
    }
}

pub struct CGAController {
    reg: [u8; 25],
    next_read: Option<CGARegister>
}

impl CGAController {
    pub fn new() -> CGAController{
        CGAController {
            reg: [0; 25],
            next_read: None
        }
    }
}

impl IoDevice for CGAController {
    const PORT_COUNT: u16 =  0x10;

    fn read_u8(&mut self, port: u16) -> u8 {
        match port {
            1 => match self.next_read {
                Some(reg) => self.reg[reg as usize],
                None => panic!("not expecting cga read")
            },
            _ => panic!("unexpected cga read")
        }
    }

    fn write_u8(&mut self, port: u16, data: u8) {
        match port {
            0 => {
                self.next_read = CGARegister::from_u8(data);
            },
            _ => panic!("unexpected cga write")
        }
    }
}
