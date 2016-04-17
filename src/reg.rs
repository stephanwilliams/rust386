use numlib::traits::{ FromPrimitive, ToPrimitive };
use num::{ Num, Size };

bitflags! {
    pub flags EFlags: u32 {
        const EFLAGS_CF         = 0b00000000000000000000000000000001,
        const EFLAGS_RESERVED1  = 0b00000000000000000000000000000010,
        const EFLAGS_PF         = 0b00000000000000000000000000000100,
        const EFLAGS_RESERVED3  = 0b00000000000000000000000000001000,
        const EFLAGS_AF         = 0b00000000000000000000000000010000,
        const EFLAGS_RESERVED5  = 0b00000000000000000000000000100000,
        const EFLAGS_ZF         = 0b00000000000000000000000001000000,
        const EFLAGS_SF         = 0b00000000000000000000000010000000,
        const EFLAGS_TF         = 0b00000000000000000000000100000000,
        const EFLAGS_IF         = 0b00000000000000000000001000000000,
        const EFLAGS_DF         = 0b00000000000000000000010000000000,
        const EFLAGS_OF         = 0b00000000000000000000100000000000,
        const EFLAGS_IOPL       = 0b00000000000000000011000000000000,
        const EFLAGS_NT         = 0b00000000000000000100000000000000,
        const EFLAGS_RESERVED14 = 0b00000000000000001000000000000000,
        const EFLAGS_RF         = 0b00000000000000010000000000000000,
        const EFLAGS_VM         = 0b00000000000000100000000000000000,
        const EFLAGS_RESERVED   = 0b11111111111111000000000000000000
    }
}

pub trait RegEnum : FromPrimitive + ToPrimitive {
    fn encode(&self) -> u8;
    fn decode(encoding: u8, size: Size) -> Self;

    fn size(&self) -> Size {
        match self.to_u32().unwrap() & 0b11 {
            0b00 | 0b01 => Size::Size8,
            0b10 => Size::Size16,
            0b11 => Size::Size32,
            _ => unreachable!()
        }
    }
}

enum_primitive! {
    #[derive(PartialEq, Eq, Debug, Copy, Clone)]
    pub enum SegmentRegister {
        ES = 0b000_1_0,
        CS = 0b001_1_0,
        SS = 0b010_1_0,
        DS = 0b011_1_0,
        FS = 0b100_1_0,
        GS = 0b101_1_0
    }
}

impl RegEnum for SegmentRegister {
    fn encode(&self) -> u8 {
        (*self as u8) >> 2
    }

    fn decode(encoding: u8, size: Size) -> SegmentRegister {
        match size {
            Size::Size16 => match encoding {
                0b000 => SegmentRegister::ES,
                0b001 => SegmentRegister::CS,
                0b010 => SegmentRegister::SS,
                0b011 => SegmentRegister::DS,
                0b100 => SegmentRegister::FS,
                0b101 => SegmentRegister::GS,
                _ => panic!("invalid segment register")
            },
            _ => panic!("invalid segment register size")
        }
    }
}

enum_primitive! {
    #[derive(PartialEq, Eq, Debug, Copy, Clone)]
    pub enum Register {
        AL  = 0b000_0_0,
        CL  = 0b001_0_0,
        DL  = 0b010_0_0,
        BL  = 0b011_0_0,

        AH  = 0b000_0_1,
        CH  = 0b001_0_1,
        DH  = 0b010_0_1,
        BH  = 0b011_0_1,

        AX  = 0b000_1_0,
        CX  = 0b001_1_0,
        DX  = 0b010_1_0,
        BX  = 0b011_1_0,
        SP  = 0b100_1_0,
        BP  = 0b101_1_0,
        SI  = 0b110_1_0,
        DI  = 0b111_1_0,

        EAX = 0b000_1_1,
        ECX = 0b001_1_1,
        EDX = 0b010_1_1,
        EBX = 0b011_1_1,
        ESP = 0b100_1_1,
        EBP = 0b101_1_1,
        ESI = 0b110_1_1,
        EDI = 0b111_1_1,
    }
}

impl RegEnum for Register {
    fn encode(&self) -> u8 {
        (*self as u8) >> 2
    }

    fn decode(encoding: u8, size: Size) -> Register {
        trace!("DECODE REGISTER FROM {:8b}", encoding);
        match size {
            Size::Size8 => {
                match encoding {
                    0b000 => Register::AL,
                    0b001 => Register::CL,
                    0b010 => Register::DL,
                    0b011 => Register::BL,
                    0b100 => Register::AH,
                    0b101 => Register::CH,
                    0b110 => Register::DH,
                    0b111 => Register::BH,
                    _ => panic!("invalid register")
                }
            },
            Size::Size16 => {
                match encoding {
                    0b000 => Register::AX,
                    0b001 => Register::CX,
                    0b010 => Register::DX,
                    0b011 => Register::BX,
                    0b100 => Register::SP,
                    0b101 => Register::BP,
                    0b110 => Register::SI,
                    0b111 => Register::DI,
                    _ => panic!("invalid register")
                }
            },
            Size::Size32 => {
                match encoding {
                    0b000 => Register::EAX,
                    0b001 => Register::ECX,
                    0b010 => Register::EDX,
                    0b011 => Register::EBX,
                    0b100 => Register::ESP,
                    0b101 => Register::EBP,
                    0b110 => Register::ESI,
                    0b111 => Register::EDI,
                    _ => panic!("invalid register")
                }
            },
            Size::Size48 => panic!("invalid size")
        }
    }
}

pub struct RegisterFile<T> where T: RegEnum {
    regs: Vec<u32>,
    dummy: Option<T>
}

impl<T> RegisterFile<T> where T: RegEnum {
    pub fn new(size: u8) -> RegisterFile<T> where T: RegEnum {
        RegisterFile {
            regs: vec![0; size as usize],
            dummy: None
        }
    }

    pub fn read(&self, reg: T) -> Num {
        let enc = reg.to_u32().unwrap();
        let ind = (enc >> 2) as usize;
        let val = self.regs[ind];

        let a = match enc & 0x3 {
            0b00 => val & 0xFF,
            0b01 => (val >> 8) & 0xFF,
            0b10 => val & 0xFFFF,
            0b11 => val,
            _ => panic!("invalid register")
        };
        // trace!("read reg {:03b} as {:08x}", ind, a);
        Num(a, reg.size().unsigned())
    }

    pub fn write(&mut self, reg: T, val: u32) {
        let enc = reg.to_u32().unwrap();
        let ind = (enc >> 2) as usize;

        assert!(match enc & 0x3 {
            0b00 => val & 0xFFFFFF00,
            0b01 => val & 0xFFFF00FF,
            0b10 => val & 0xFFFF0000,
            _ => 0
        } == 0, "reg value out of range");

        self.regs[ind] = match enc & 0x3 {
            0b00 => (self.regs[ind] & 0xFFFFFF00) | val,
            0b01 => (self.regs[ind] & 0xFFFF00FF) | (val << 8),
            0b10 => (self.regs[ind] & 0xFFFF0000) | val,
            0b11 => val,
            _ => panic!("invalid register")
        };
        trace!("write reg {:03b} as {:08x}", ind, self.regs[ind]);
    }
}
