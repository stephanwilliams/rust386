use numlib::traits::ToPrimitive;

use opcodes::{
    UnresolvedOperands, UnresolvedOperand, UnresolvedRegister, UnresolvedOp,
    AddressingMethod, OperantType, GROUP_MAP
};
use reg::{
    Register, SegmentRegister, ControlRegister,
    DebugRegister, TestRegister, RegEnum
};
use num::{ Size };

// base + ind * scale + disp
// Option<Register> Option<Register> Size Size

macro_rules! addr_form {
    ($size:ident, $oseg:ident, $seg:ident,
     [--]) => {
        Op::MemoryAddress(
            $oseg.unwrap_or(SegmentRegister::$seg),
            None,
            None,
            1,
            None,
            $size
        )
    };

    ($size:ident, $oseg:ident, $seg:ident,
     [--] + $off:ident) => {
        Op::MemoryAddress(
            $oseg.unwrap_or(SegmentRegister::$seg),
            None,
            None,
            1,
            Some(Size::$off),
            $size
        )
    };

    ($size:ident, $oseg:ident, $seg:ident,
     [$base:ident + $ind:ident * $scale:expr] + $off:ident) => {
        Op::MemoryAddress(
            $oseg.unwrap_or(SegmentRegister::$seg),
            Some(Register::$base),
            Some(Register::$ind),
            $scale,
            Some(Size::$off),
            $size
        )
    };

    ($size:ident, $oseg:ident, $seg:ident,
     [$base:ident + $ind:ident * $scale:expr]) => {
        Op::MemoryAddress(
            $oseg.unwrap_or(SegmentRegister::$seg),
            Some(Register::$base),
            Some(Register::$ind),
            $scale,
            None,
            $size
        )
    };

    ($size:ident, $oseg:ident, $seg:ident,
     [$base:ident + $ind:ident] + $off:ident) => {
        Op::MemoryAddress(
            $oseg.unwrap_or(SegmentRegister::$seg),
            Some(Register::$base),
            Some(Register::$ind),
            1,
            Some(Size::$off),
            $size
        )
    };

    ($size:ident, $oseg:ident, $seg:ident,
     [$base:ident + $ind:ident]) => {
        Op::MemoryAddress(
            $oseg.unwrap_or(SegmentRegister::$seg),
            Some(Register::$base),
            Some(Register::$ind),
            1,
            None,
            $size
        )
    };

    ($size:ident, $oseg:ident, $seg:ident,
     [$base:ident]) => {
        Op::MemoryAddress(
            $oseg.unwrap_or(SegmentRegister::$seg),
            Some(Register::$base),
            None,
            1,
            None,
            $size
        )
    };

    ($size:ident, $oseg:ident, $seg:ident,
     [$base:ident] + $off:ident) => {
        Op::MemoryAddress(
            $oseg.unwrap_or(SegmentRegister::$seg),
            Some(Register::$base),
            None,
            1,
            Some(Size::$off),
            $size
        )
    };

    ($size:ident, $oseg:ident, $seg:ident,
     $off:ident) => {
        Op::MemoryAddress(
            $oseg.unwrap_or(SegmentRegister::$seg),
            None,
            None,
            1,
            Some(Size::$off),
            $size
        )
    };
}


#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum InstructionPrefix {
    None,
    REP,
    REPN,
    LOCK
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum SegmentOverridePrefix {
    None,
    CS,
    SS,
    DS,
    ES,
    FS,
    GS
}

impl SegmentOverridePrefix {
    pub fn to_seg_reg(self) -> Option<SegmentRegister> {
        match self {
            SegmentOverridePrefix::CS => Some(SegmentRegister::CS),
            SegmentOverridePrefix::SS => Some(SegmentRegister::SS),
            SegmentOverridePrefix::DS => Some(SegmentRegister::DS),
            SegmentOverridePrefix::ES => Some(SegmentRegister::ES),
            SegmentOverridePrefix::FS => Some(SegmentRegister::FS),
            SegmentOverridePrefix::GS => Some(SegmentRegister::GS),
            SegmentOverridePrefix::None => None
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub instr_prefix: InstructionPrefix,

    pub addr_sz_prefix: bool,
    pub addr_sz: Size,

    pub op_sz_prefix: bool,
    pub op_sz: Size,

    pub seg_override_prefix: SegmentOverridePrefix,

    pub opcode: u16,
    pub unresolved_operands: UnresolvedOperands,

    pub modrm: Option<u8>,
    pub sib: Option<u8>,

    pub disp: u32,
    pub disp_sz: u32,
    pub disp_sz_set: bool,

    pub imm: u32,
    pub imm_sz: u32,
    pub imm_sz_set: bool,

    pub operands: Operands
}

impl Instruction {
    pub fn new() -> Instruction {
        Instruction {
            instr_prefix: InstructionPrefix::None,
            addr_sz_prefix: false,
            addr_sz: Size::Size16,
            op_sz_prefix: false,
            op_sz: Size::Size16,
            seg_override_prefix: SegmentOverridePrefix::None,
            opcode: 0,
            unresolved_operands: UnresolvedOperands::Invalid,
            modrm: None,
            sib: None,
            disp: 0,
            disp_sz: 0,
            disp_sz_set: false,
            imm: 0,
            imm_sz: 0,
            imm_sz_set: false,
            operands: Operands::None
        }
    }

    pub fn has_sib(&self) -> bool {
        if self.addr_sz == Size::Size32 {
            if let Some(modrm) = self.modrm {
                return match modrm & 0b11_000_111 {
                    0b00_000_100 |
                    0b01_000_100 |
                    0b10_000_100 => true,
                    _ => false
                };
            }
        }

        false
    }

    pub fn has_modrm(&self) -> bool {
        self.unresolved_operands.has_modrm()
    }

    pub fn modrm_mod(&self) -> Option<u8> {
        if let Some(modrm) = self.modrm {
            Some((modrm >> 6) & 0x3)
        } else {
            None
        }
    }

    pub fn modrm_regop(&self) -> Option<u8> {
        if let Some(modrm) = self.modrm {
            Some((modrm >> 3) & 0x7)
        } else {
            None
        }
    }

    pub fn modrm_rm(&self) -> Option<u8> {
        if let Some(modrm) = self.modrm {
            Some(modrm & 0x7)
        } else {
            None
        }
    }

    pub fn sib_ss(&self) -> Option<u8> {
        if let Some(sib) = self.sib {
            Some((sib >> 6) & 0x3)
        } else {
            None
        }
    }

    pub fn sib_index(&self) -> Option<u8> {
        if let Some(sib) = self.sib {
            Some((sib >> 3) & 0x7)
        } else {
            None
        }
    }

    pub fn sib_base(&self) -> Option<u8> {
        if let Some(sib) = self.sib {
            Some(sib & 0x7)
        } else {
            None
        }
    }

    fn set_disp_sz_once(&mut self, size: Size) {
        assert!(!self.disp_sz_set);
        self.disp_sz = size.to_u32().unwrap();
        self.disp_sz_set = true;
    }

    fn set_imm_sz_once(&mut self, size: Size) {
        assert!(!self.imm_sz_set);
        self.imm_sz = size.to_u32().unwrap();
        self.imm_sz_set = true;
    }

    pub fn resolve(&mut self) {
        self.operands = match self.unresolved_operands {
            UnresolvedOperands::None => Operands::None,
            UnresolvedOperands::Single(uop1) => Operands::Single(
                self.resolve_op(uop1)
                ),
            UnresolvedOperands::Double(uop1, uop2) => Operands::Double(
                self.resolve_op(uop1),
                self.resolve_op(uop2)
                ),
            UnresolvedOperands::Triple(uop1, uop2, uop3) => Operands::Triple(
                self.resolve_op(uop1),
                self.resolve_op(uop2),
                self.resolve_op(uop3)
                ),
            UnresolvedOperands::Group(grp) =>
                self.resolve_grp(grp, UnresolvedOperands::None),
            UnresolvedOperands::GroupSingle(grp, uop1) =>
                self.resolve_grp(grp, UnresolvedOperands::Single(uop1)),
            UnresolvedOperands::GroupDouble(grp, uop1, uop2) =>
                self.resolve_grp(grp, UnresolvedOperands::Double(uop1, uop2)),
            _ => panic!("resolve invalid instr")
        };
    }

    fn resolve_grp(&mut self, grp: usize, grp_ops: UnresolvedOperands) -> Operands {
        let op = self.modrm_regop().unwrap() as usize;
        let grp_ind = if grp == 3 && self.opcode == 0xf7 { 8 } else { grp - 1 };
        let ugrp = GROUP_MAP[grp_ind][op];
        match grp_ops {
            UnresolvedOperands::None =>
                match ugrp {
                    UnresolvedOperands::None => Operands::None,
                    UnresolvedOperands::Single(guop1) => Operands::Single(
                        self.resolve_op(guop1)
                        ),
                    UnresolvedOperands::Double(guop1, guop2) => Operands::Double(
                        self.resolve_op(guop1),
                        self.resolve_op(guop2)
                        ),
                    UnresolvedOperands::Triple(guop1, guop2, guop3) => Operands::Triple(
                        self.resolve_op(guop1),
                        self.resolve_op(guop2),
                        self.resolve_op(guop3)
                        ),
                    _ => panic!("resolve invalid instr")
                },
            UnresolvedOperands::Single(uop1) =>
                match ugrp {
                    UnresolvedOperands::None => Operands::Single(
                        self.resolve_op(uop1)
                        ),
                    UnresolvedOperands::Single(guop1) => Operands::Double(
                        self.resolve_op(uop1),
                        self.resolve_op(guop1)
                        ),
                    UnresolvedOperands::Double(guop1, guop2) => Operands::Triple(
                        self.resolve_op(uop1),
                        self.resolve_op(guop1),
                        self.resolve_op(guop2)
                        ),
                    _ => panic!("resolve invalid instr")
                },
            UnresolvedOperands::Double(uop1, uop2) =>
                match ugrp {
                    UnresolvedOperands::None => Operands::Double(
                        self.resolve_op(uop1),
                        self.resolve_op(uop2)
                        ),
                    UnresolvedOperands::Single(guop1) => Operands::Triple(
                        self.resolve_op(uop1),
                        self.resolve_op(uop2),
                        self.resolve_op(guop1)
                        ),
                    _ => panic!("resolve invalid instr")
                },
            UnresolvedOperands::Triple(uop1, uop2, uop3) =>
                match ugrp {
                    UnresolvedOperands::None => Operands::Triple(
                        self.resolve_op(uop1),
                        self.resolve_op(uop2),
                        self.resolve_op(uop3)
                        ),
                    _ => panic!("resolve invalid instr")
                },
            _ => panic!("resolve invalid instr")
        }
    }

    fn resolve_op(&mut self, op: UnresolvedOp) -> Op {
        match op {
            UnresolvedOp::Operand(oper) => self.resolve_operand(oper),
            UnresolvedOp::Register(reg) => self.resolve_reg(reg),
            UnresolvedOp::Constant(con) => Op::Constant(con)
        }
    }

    fn resolve_operand(&mut self, op: UnresolvedOperand) -> Op {
        let opty = self.resolve_operand_type(op.op_type);
        let first = opty.0;
        match op.addr_method {
            // No ModR/M byte; address of operand is encoded in instruction
            // No base register/index register/scaling factor can be applied (e.g. far JMP EA)
            AddressingMethod::A =>
                if opty.1.is_some() {
                    self.set_disp_sz_once(opty.0);
                    self.set_imm_sz_once(opty.1.unwrap());
                    Op::Memory2(opty.0, opty.1.unwrap())
                } else {
                    self.set_disp_sz_once(opty.0);
                    Op::Memory1(opty.0)
                },
            // Reg field of ModR/M byte selects a control register
            AddressingMethod::C => match self.modrm_mod().unwrap() {
                0b11 => Op::ControlRegister(ControlRegister::decode(self.modrm_regop().unwrap(), first)),
                _ => panic!("invalid modrm mod")
            },
            // Reg field of ModR/M byte selects a debug register
            AddressingMethod::D => match self.modrm_mod().unwrap() {
                0b11 => Op::DebugRegister(DebugRegister::decode(self.modrm_regop().unwrap(), first)),
                _ => panic!("invalid modrm mod")
            },
            // ModR/M specifies operand; gen reg or mem addr. If mem addr,
            // address is computed from seg reg and: base reg, index reg, scaling factor, or
            // displacement
            AddressingMethod::E => match self.modrm_mod().unwrap() {
                0b00 | 0b01 | 0b10 => self.resolve_addr_form(first),
                0b11 => Op::Register(Register::decode(self.modrm_rm().unwrap(), first)),
                _ => panic!("invalid modrm mod")
            },
            // flags register
            AddressingMethod::F => Op::FlagsRegister(first),
            // Reg field of ModR/M byte selects gen reg
            AddressingMethod::G => Op::Register(
                Register::decode(self.modrm_regop().unwrap(), first)),
            // Immediate data
            AddressingMethod::I => {
                self.set_imm_sz_once(first);
                Op::Immediate(first)
            },
            // Instr contains relative offset to be added to eIP (short JMP, LOOP)
            AddressingMethod::J => {
                self.set_imm_sz_once(first);
                Op::RelativeAddress(first)
            },
            // ModR/M may only refer to memory (BOUND, LES, LDS, LSS, LFS, LGS)
            AddressingMethod::M => match self.modrm_mod().unwrap() {
                0b00 | 0b01 | 0b10 => self.resolve_addr_form(first),
                _ => panic!("invalid modrm mod")
            },
            // No ModR/M; offset of operand coded as (d)word dep on addr sz
            // No base reg, index reg, scale factor
            AddressingMethod::O => {
                let addr_sz = self.addr_sz;
                self.set_disp_sz_once(addr_sz);
                Op::MemoryAddress(self.seg_override_prefix.to_seg_reg()
                           .unwrap_or(SegmentRegister::DS),
                           None,
                           None,
                           1,
                           Some(addr_sz),
                           first)
            },
            // Mod field of ModR/M may refer only to gen reg
            AddressingMethod::R => match self.modrm_mod().unwrap() {
                0b11 => Op::Register(Register::decode(self.modrm_rm().unwrap(), first)),
                _ => panic!("invalid modrm mod")
            },
            // Reg field of ModR/M selects segment register
            AddressingMethod::S => Op::SegmentRegister(
                SegmentRegister::decode(self.modrm_regop().unwrap(), first)),
            // Reg field of ModR/M selects test register
            AddressingMethod::T => match self.modrm_regop().unwrap() {
                0b11 => Op::TestRegister(TestRegister::decode(self.modrm_rm().unwrap(), first)),
                _ => panic!("invalid modrm mod")
            },
            // Memory addressed by DS:SI (MOVS, COMPS, OUTS, LODS, SCAS)
            AddressingMethod::X => Op::MemoryAddress(
                SegmentRegister::DS,
                Some(Register::decode(0b110, self.addr_sz)), // eSI
                None,
                1,
                None,
                first 
                ),
            // Memory addressed by ES:DI (MOVS, CMPS, INS, STOS)
            AddressingMethod::Y => Op::MemoryAddress(
                SegmentRegister::ES,
                Some(Register::decode(0b111, self.addr_sz)), // eDI
                None,
                1,
                None,
                first 
                )
        }
    }

    fn resolve_operand_type(&self, opty: OperantType) -> (Size, Option<Size>) {
        let op16 = self.op_sz == Size::Size16;
        match opty {
            // a: Two one-word operands in memory or two double-word operands
            //    in memory, depending on operand size attribute (used only by
            //    BOUND).
            OperantType::a => if op16 {
                (Size::Size8, Some(Size::Size8))
            } else {
                (Size::Size16, Some(Size::Size16))
            },
            // b: Byte (regardless of operand size attribute).
            OperantType::b => (Size::Size8, None),
            // c: Byte or word, depending on operand size attribute.
            OperantType::c => if op16 {
                (Size::Size8, None)
            } else {
                (Size::Size8, None)
            },
            // Double word (regardless of operand size attribute).
            OperantType::d => (Size::Size32, None),
            // p: 32-bit or 48-bit pointer, depending on operand size attribute.
            OperantType::p => if op16 {
                (Size::Size16, Some(Size::Size16))
            } else {
                (Size::Size32, Some(Size::Size16))
            },
            // Six-bye pseudo-descriptor.
            OperantType::s => (Size::Size48, None),
            // Word or double word, depending on operand size attribute.
            OperantType::v => if op16 {
                (Size::Size16, None)
            } else {
                (Size::Size32, None)
            },
            // Word (regardless of operand size attribute).
            OperantType::w => (Size::Size16, None),
        }
    }

    fn resolve_reg(&self, ureg: UnresolvedRegister) -> Op {
        let op16 = self.op_sz == Size::Size16;
        match ureg {
            UnresolvedRegister::eAX =>
                Op::Register(if op16 { Register::AX } else { Register::EAX }),
            UnresolvedRegister::eCX =>
                Op::Register(if op16 { Register::CX } else { Register::ECX }),
            UnresolvedRegister::eDX =>
                Op::Register(if op16 { Register::DX } else { Register::EDX }),
            UnresolvedRegister::eBX =>
                Op::Register(if op16 { Register::BX } else { Register::EBX }),
            UnresolvedRegister::eSP =>
                Op::Register(if op16 { Register::SP } else { Register::ESP }),
            UnresolvedRegister::eBP =>
                Op::Register(if op16 { Register::BP } else { Register::EBP }),
            UnresolvedRegister::eSI =>
                Op::Register(if op16 { Register::SI } else { Register::ESI }),
            UnresolvedRegister::eDI =>
                Op::Register(if op16 { Register::DI } else { Register::EDI }),
            UnresolvedRegister::DX => Op::Register(Register::DX),
            UnresolvedRegister::AL => Op::Register(Register::AL),
            UnresolvedRegister::CL => Op::Register(Register::CL),
            UnresolvedRegister::DL => Op::Register(Register::DL),
            UnresolvedRegister::BL => Op::Register(Register::BL),
            UnresolvedRegister::AH => Op::Register(Register::AH),
            UnresolvedRegister::CH => Op::Register(Register::CH),
            UnresolvedRegister::DH => Op::Register(Register::DH),
            UnresolvedRegister::BH => Op::Register(Register::BH),

            UnresolvedRegister::ES => Op::SegmentRegister(SegmentRegister::ES),
            UnresolvedRegister::SS => Op::SegmentRegister(SegmentRegister::SS),
            UnresolvedRegister::CS => Op::SegmentRegister(SegmentRegister::CS),
            UnresolvedRegister::DS => Op::SegmentRegister(SegmentRegister::DS),
        }
    }

    fn sib_index_to_reg(&self) -> Option<Register> {
        if let Some(index) = self.sib_index() {
            return match index {
                0b000 => Some(Register::EAX),
                0b001 => Some(Register::ECX),
                0b010 => Some(Register::EDX),
                0b011 => Some(Register::EBX),
                0b100 => None,
                0b101 => Some(Register::EBP),
                0b110 => Some(Register::ESI),
                0b111 => Some(Register::EDI),
                _ => panic!("invalid sib index")
            };
        }

        None
    }

    fn sib_ss_to_size(&self) -> u8 {
        1 << self.sib_ss().unwrap()
    }

    fn resolve_addr_form(&mut self, size: Size) -> Op {
        let modrm_mod = self.modrm_mod().unwrap();
        let modrm_rm = self.modrm_rm().unwrap();
        let seg = 
            self.sib_index_to_reg()
            .and_then(|reg| match reg {
                Register::EBP => Some(SegmentRegister::SS),
                _ => None
            })
            .or(self.seg_override_prefix.to_seg_reg());

        assert!(modrm_mod != 0b11);

        let ss = self.sib_ss();

        let mut addr_form = (match self.addr_sz {
            Size::Size16 => [[
                addr_form!(size, seg, DS, [BX + SI]),
                addr_form!(size, seg, DS, [BX + DI]),
                addr_form!(size, seg, SS, [BP + SI]),
                addr_form!(size, seg, SS, [BP + DI]),
                addr_form!(size, seg, DS, [SI]),
                addr_form!(size, seg, DS, [DI]),
                addr_form!(size, seg, DS, Size16),
                addr_form!(size, seg, DS, [BX]),
            ],[
                addr_form!(size, seg, DS, [BX + SI] + Size8),
                addr_form!(size, seg, DS, [BX + DI] + Size8),
                addr_form!(size, seg, SS, [BP + SI] + Size8),
                addr_form!(size, seg, SS, [BP + DI] + Size8),
                addr_form!(size, seg, DS, [SI] + Size8),
                addr_form!(size, seg, DS, [DI] + Size8),
                addr_form!(size, seg, SS, [BP] + Size8),
                addr_form!(size, seg, DS, [BX] + Size8),
            ],[
                addr_form!(size, seg, DS, [BX + SI] + Size16),
                addr_form!(size, seg, DS, [BX + DI] + Size16),
                addr_form!(size, seg, SS, [BP + SI] + Size16),
                addr_form!(size, seg, SS, [BP + DI] + Size16),
                addr_form!(size, seg, DS, [SI] + Size16),
                addr_form!(size, seg, DS, [DI] + Size16),
                addr_form!(size, seg, SS, [BP] + Size16),
                addr_form!(size, seg, DS, [BX] + Size16),
            ]],
            Size::Size32 => [[
                addr_form!(size, seg, DS, [EAX]),
                addr_form!(size, seg, DS, [ECX]),
                addr_form!(size, seg, DS, [EDX]),
                addr_form!(size, seg, DS, [EBX]),
                addr_form!(size, seg, DS, [--]), // !!!!!!
                addr_form!(size, seg, DS, Size32),
                addr_form!(size, seg, DS, [ESI]),
                addr_form!(size, seg, DS, [EDI]),
            ],[
                addr_form!(size, seg, DS, [EAX] + Size8),
                addr_form!(size, seg, DS, [ECX] + Size8),
                addr_form!(size, seg, DS, [EDX] + Size8),
                addr_form!(size, seg, DS, [EBX] + Size8),
                addr_form!(size, seg, DS, [--] + Size8), // !!!!!
                addr_form!(size, seg, SS, [EBP] + Size8),
                addr_form!(size, seg, DS, [ESI] + Size8),
                addr_form!(size, seg, DS, [EDI] + Size8),
            ],[
                addr_form!(size, seg, DS, [EAX] + Size32),
                addr_form!(size, seg, DS, [ECX] + Size32),
                addr_form!(size, seg, DS, [EDX] + Size32),
                addr_form!(size, seg, DS, [EBX] + Size32),
                addr_form!(size, seg, DS, [--] + Size32), // !!!!!
                addr_form!(size, seg, SS, [EBP] + Size32),
                addr_form!(size, seg, DS, [ESI] + Size32),
                addr_form!(size, seg, DS, [EDI] + Size32),
            ]],
            _ => panic!("invalid operand size for addr form")
        })[modrm_mod as usize][modrm_rm as usize];

        // trace!("modrm mod {:02b} rm {:03b} op {:?}", modrm_mod, modrm_rm, addr_form);
        // trace!("modrm {:0x} {:08b}", self.modrm.unwrap(), self.modrm.unwrap());
        
        // sib
        if self.addr_sz == Size::Size32 && modrm_rm == 0b100 {
            if let Op::MemoryAddress(seg, _, _, _, disp, size) = addr_form {
                let scale = self.sib_ss().unwrap();
                let index = self.sib_index_to_reg();
                let base = match Register::decode(self.sib_base().unwrap(), Size::Size32) {
                    reg @ Register::EBP => if modrm_mod == 0b00 { None } else { Some(reg) },
                    reg @ _ => Some(reg)
                };
                addr_form = Op::MemoryAddress(seg, base, index, scale, disp, size);
            }
        }

        if let Op::MemoryAddress(_, _, _, _, Some(disp), _) = addr_form {
            self.set_disp_sz_once(disp);
        }

        addr_form
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Op {
    Register(Register),
    SegmentRegister(SegmentRegister),
    ControlRegister(ControlRegister),
    DebugRegister(DebugRegister),
    TestRegister(TestRegister),
    FlagsRegister(Size),
    Memory1(Size),
    Memory2(Size, Size),
    MemoryAddress(
        SegmentRegister,
        Option<Register>,
        Option<Register>,
        u8,
        Option<Size>,
        Size),
    Immediate(Size),
    RelativeAddress(Size),
    MemoryOffset(Size),
    Pointer(Size),
    Constant(u32),
    Offset(Size)
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Operands {
    None,
    Single(Op),
    Double(Op, Op),
    Triple(Op, Op, Op),
}
