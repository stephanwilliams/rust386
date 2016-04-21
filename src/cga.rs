use numlib::traits::{ FromPrimitive };
use piston_window::*;

use std::path::Path;
use std::str;


use iobus::{ IoDevice };

pub const CRT_ROWS: u32 = 25;
pub const CRT_COLS: u32 = 80;

pub struct CGA {
    ram: [u8; 0x8000],
    window: PistonWindow
}

impl CGA {
    pub fn new() -> CGA {
        let mut cga = CGA {
            ram: [0; 0x8000],
            window: WindowSettings::new("rust386", (1024, 768))
                .exit_on_esc(true)
                .build()
                .unwrap_or_else(|e| { panic!("Failed to build PistonWindow: {}", e) })
        };

        for row in 0..CRT_ROWS {
            for col in 0..CRT_COLS {
                let off = (row * CRT_COLS + col) as usize;
                cga.ram[2 * off] = 0x20;
                cga.ram[2 * off + 1] = 0x70;
            }
        }

        cga
    }

    pub fn read_u8(&self, addr: u32) -> u8 {
        let data = self.ram[addr as usize];
        
        data
    }

    pub fn read_u16(&self, addr: u32) -> u16 {
        let high = self.read_u8(addr + 1) as u16;
        let low = self.read_u8(addr) as u16;
        let data = (high << 8) | low;
        trace!("cga read {:02x} from {:08x}", data, addr);

        data
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        let hh = self.read_u8(addr + 3) as u32;
        let hl = self.read_u8(addr + 2) as u32;
        let lh = self.read_u8(addr + 1) as u32;
        let ll = self.read_u8(addr) as u32;
        let data = (hh << 24) | (hl << 16) | (lh << 8) | ll;
        trace!("cga read {:02x} from {:08x}", data, addr);

        data
    }

    pub fn write_u8(&mut self, addr: u32, data: u8) {
        // println!("cga write {:02x} to {:08x}", data, addr);
        self.ram[addr as usize] = data;
    }

    fn write_u16(&mut self, addr: u32, data: u16) {
        self.write_u8(addr, data as u8);
        self.write_u8(addr + 1, (data >> 8) as u8);
    }

    pub fn write_u32(&mut self, addr: u32, data: u32) {
        trace!("cga read {:08x} from {:08x}", data, addr);
        self.write_u8(addr, data as u8);
        self.write_u8(addr + 1, (data >> 8) as u8);
        self.write_u8(addr + 2, (data >> 16) as u8);
        self.write_u8(addr + 3, (data >> 24) as u8);
    }

    pub fn update(&mut self) -> Option<()> {
        if let Some(window) = self.window.next() {
            self.window = window;

            let t = Text::new_color(color::WHITE, 16);
            let mut cache = Glyphs::new(
                &Path::new("/Library/Fonts/Courier New.ttf"),
                self.window.factory.borrow_mut().clone()
                ).ok().unwrap();

            self.window.draw_2d(|_c, g| {
                clear([0.2, 0.2, 0.2, 1.0], g);
                for row in 0..CRT_ROWS {
                    for col in 0..CRT_COLS {
                        let off = 2 * (row * CRT_COLS + col);
                        let ch = self.read_u8(off);
                        let charr = [ch];
                        let s = str::from_utf8(&charr);
                        if !s.is_ok() { continue; }
                        t.draw(
                            s.unwrap(),
                            &mut cache,
                            &_c.draw_state,
                            _c.transform.trans(
                                32.0 + 12.0 * col as f64,
                                30.0 * (row + 1) as f64),
                            g);
                    }
                }
            });

            return Some(());
        }

        None
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
        0
        // match port {
        //     5 => match self.next_read {
        //         Some(reg) => self.reg[reg as usize],
        //         None => panic!("not expecting cga read")
        //     },
        //     _ => panic!("unexpected cga read")
        // }
    }

    fn write_u8(&mut self, port: u16, data: u8) {
        // match port {
        //     4 => {
        //         self.next_read = CGARegister::from_u8(data);
        //     },
        //     _ => panic!("unexpected cga write {} to port {}", data, port)
        // }
    }
}
