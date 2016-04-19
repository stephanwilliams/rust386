use numlib::traits::{ FromPrimitive, ToPrimitive };

use std::mem;
use std::ops::{ Range, };

use clock::{ Clocked };
use bus::{ BusState, BusLine, Signal };
use cache::{ Cache };
use rom::{ Rom };
use opcodes::{
    UnresolvedOperands, SINGLE_OPCODE_MAP, DOUBLE_OPCODE_MAP
};
use reg::{
    SegmentRegister, Register, RegisterFile, EFlags, RegEnum,
    ControlRegister, TestRegister, DebugRegister, CR0
};
use instr::{
    InstructionPrefix, SegmentOverridePrefix, Instruction,
    Operands, Op
};
use num::{ Num, Size, Type };
use cga::{ CGA };

type VAddr  = u32;
type SAddr  = (u16, u16);
type PAddr  = u32;
type IOAddr = u16;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum BusTransferState {
    Idle,
    T1,
    T2
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum ProcessorMode {
    Real,
    Protected
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum InstructionState {
    Fetch0,
    Fetch1,
    Fetch2,
    Fetch3,
    Fetch4,
    Fetch5,
    Fetch6,
    Fetch7,
    Fetch8,
    Fetch9,
    Fetch10,
    Fetch11,
    Fetch12,
    Fetch13,
    Fetch14,
    Fetch15,
    Decode,
    Dispatch,

    GEN_OP_Read,
    GEN_OP_Write,

    GRP1,
    GRP2,
    GRP3,
    GRP4,
    GRP5,
    GRP6,
    GRP7,

    ADD_0,
    ADC_1,
    AND_2,
    XOR_3,
    OR_0,
    SBB_1,
    SUB_2,
    CMP_3,

    INC,
    DEC,

    ROL,
    ROR,
    RCL,
    RCR,
    SHL,
    SHR,
    SAR,

    PUSH,
    POP,

    INS_6x,
    Jcc_7x,
    MOV,
    LEA,

    TEST_Ax,

    RET_C3,

    IN_0,
    OUT_0,

    CALL_E8,
    JMP_EA,
    JMP_EB,

    CLC_F8,
    STC_F9,
    CLI_FA,
    STI_FB,
    CLD_FC,
    STD_FD,

    SGDT,
    SIDT,
    LGDT,
    LIDT,
    SMSW,
    LMSW,

    CALL_FF2,

    MOV_0F2x,
    MOVZX_0FBx,

    HALT,

    Post
}

macro_rules! segaddr {
    ( $slf:ident, $seg:ident : $off:ident ) => {
        ($slf.seg_regs.read(SegmentRegister::$seg)._as(Type::u32) << 4) | $slf.gen_regs.read(Register::$off)
    };

    ( $slf:ident, $seg:ident : $off:expr ) => {
        ($slf.seg_regs.read(SegmentRegister::$seg)._as(Type::u32) << 4) | $off
    }
}

macro_rules! fetch_next_instr_byte {
    ( $slf:ident ) => {{
        trace!("fetch next instr byte");
        let mut dist: i32 = ($slf.instr_eip as i32) - ($slf.instr_buf_addr as i32);
        if dist < 0 || dist >= 4 {
            $slf.instr_buf_addr = $slf.instr_eip & !0x3;
            let addr_copy = $slf.instr_buf_addr;
            if let Some(new_buf) = $slf.mmu_read_mem_u32_aligned(addr_copy) {
                $slf.instr_buf = new_buf;
                dist = ($slf.instr_eip - $slf.instr_buf_addr) as i32;
                trace!("new instr buf: {:08x}", new_buf);
            } else {
                return;
            }
        }

        $slf.instr_eip += 1;
        $slf.instr_byte = (($slf.instr_buf >> (dist * 8)) & 0xFF) as u8;
    }};
}

macro_rules! instr_state {
    ( $slf:ident, $cur:ident -> $next:ident, $blk:block $(, -> $skp:ident if $skipif:expr)*) => {
        instr_state![$slf, $cur -> $next, $blk, consumed if false $(, -> $skp if $skipif)*];
    };

    ( $slf:ident, $cur:ident -> $next:ident, $blk:block, consumed if $exp:expr $(, -> $skp:ident if $skipif:expr)*) => {
        if $slf.instr_state == InstructionState::$cur {
            trace!("process state {:?}", InstructionState::$cur);
            if $slf.instr_did_consume_byte {
                $slf.instr_len += 1;
                fetch_next_instr_byte![$slf];
                trace!("instr byte: {:02x}", $slf.instr_byte);
            }
            $blk;
            $slf.instr_did_consume_byte = $exp;

            if $slf.instr_did_consume_byte { trace!("{:02x} CONSUMED by {:?}", $slf.instr_byte,$slf.instr_state); }
            // $(if $skipif {
            //     debug!("skip to state {:?}", InstructionState::$skp);
            //     $slf.instr_state = InstructionState::$skp;
            // } else )*
            // { $slf.instr_state = InstructionState::$next; }
            $slf.instr_state = InstructionState::$next;
            $(if $skipif {
                trace!("skip to state {:?}", InstructionState::$skp);
                $slf.instr_state = InstructionState::$skp;
            } )*
        }
    };
}

pub struct Intel80386 {
    bus_transfer_state: BusTransferState,
    instr_state: InstructionState,
    cache: Cache,
    roms: Vec<(Range<u64>, Rom)>,
    cga: Option<CGA>,

    eip: u32,
    has_ljmped: bool,
    gen_regs: RegisterFile<Register>,
    seg_regs: RegisterFile<SegmentRegister>,
    con_regs: RegisterFile<ControlRegister>,
    dbg_regs: RegisterFile<DebugRegister>,
    tst_regs: RegisterFile<TestRegister>,
    eflags: EFlags,

    instr_eip: u32,
    instr_len: u32,
    instr_byte: u8,
    instr_buf: u32,
    instr_buf_addr: u32,
    instr_did_consume_byte: bool,

    current_instr: Instruction,
    op1val: Option<Num>,
    op2val: Option<Num>,
    op3val: Option<Num>,
    result: Option<Num>,

    bus_is_write: bool, // vs write
    bus_is_data: bool, // vs control
    bus_is_mem: bool, // vs io
    bus_addr: PAddr,
    bus_data: u32,
    bus_data_size: usize,

    io_read_result: Option<(IOAddr, Num)>
}

impl Intel80386 {
    pub fn new() -> Intel80386 {
        let mut i386 = Intel80386 {
            bus_transfer_state: BusTransferState::Idle,
            instr_state: InstructionState::Fetch0,
            cache: Cache::new(),
            roms: vec![],
            cga: None,

            eip: 0,
            has_ljmped: false,
            gen_regs: RegisterFile::new(8),
            seg_regs: RegisterFile::new(6),
            con_regs: RegisterFile::new(4),
            dbg_regs: RegisterFile::new(8),
            tst_regs: RegisterFile::new(8),
            eflags: ::reg::EFLAGS_RESERVED1,

            instr_eip: 0,
            instr_len: 0,
            instr_byte: 0,
            instr_buf: 0,
            instr_buf_addr: 0,
            instr_did_consume_byte: true,

            current_instr: Instruction::new(),
            op1val: None,
            op2val: None,
            op3val: None,
            result: None,

            bus_is_write: false,
            bus_is_data: false,
            bus_is_mem: false,
            bus_addr: 0,
            bus_data: 0,
            bus_data_size: 0,

            io_read_result: None
        };

        i386.eip = 0xFFF0;

        i386.seg_regs.write(SegmentRegister::CS, 0xF000);

        i386
    }

    pub fn map_rom(&mut self, rom: Rom, range: Range<u64>) {
        assert!(range.start % 4 == 0, "rom must be 4 byte aligned");
        assert!(rom.size() % 4 == 0, "rom size must be a multiple of 4 bytes");
        assert!(range.end <= 0x100000000, "rom range too large");
        assert!(rom.size() == (range.end - range.start) as usize, "rom range wrong size");
        self.roms.push((range, rom));
    }

    pub fn set_cga(&mut self, cga: CGA) {
        self.cga = Some(cga);
    }

    pub fn get_cga(&self) -> Option<&CGA> {
        if let Some(ref cga) = self.cga {
            return Some(&cga);
        }

        None
    }

    fn instr_cycle(&mut self, state: &BusState) {
        // Instruction Prefix
        if self.instr_state == InstructionState::Fetch0 {
            self.print_state();
            self.instr_eip = self.eip().to_u32().unwrap();
            self.instr_len = 0;
            self.instr_did_consume_byte = true;
            self.current_instr = Instruction::new();
            self.op1val = None;
            self.op2val = None;
            self.op3val = None;
            self.result = None;
        }
        instr_state![self, Fetch0 -> Fetch1, {
            debug!("instr at eip: {:08x}", self.instr_eip);

            self.current_instr.instr_prefix = match self.instr_byte {
                0xF3 => InstructionPrefix::REP,
                0xF2 => InstructionPrefix::REPN,
                0xF0 => InstructionPrefix::LOCK,
                _ => InstructionPrefix::None
            };

            debug!("has instr prefix: {:?}", self.current_instr.instr_prefix);
        },
        consumed if self.current_instr.instr_prefix != InstructionPrefix::None];


        // Address Size Prefix
        instr_state![self, Fetch1 -> Fetch2, {
            self.current_instr.addr_sz_prefix = false;
            self.current_instr.addr_sz = self.default_size();
            self.current_instr.op_sz_prefix = false;
            self.current_instr.op_sz = self.default_size();

            match self.instr_byte {
                0x66 => {
                    self.current_instr.op_sz_prefix = true;
                    self.current_instr.op_sz = !self.default_size();
                },
                0x67 => {
                    self.current_instr.addr_sz_prefix = true;
                    self.current_instr.addr_sz = !self.default_size();
                },
                _ => { }
            }
        },
        consumed if self.instr_byte == 0x66 || self.instr_byte == 0x67];


        // Operand Size Prefix
        instr_state![self, Fetch2 -> Fetch3, {
            match self.instr_byte {
                0x66 => {
                    self.current_instr.op_sz_prefix = true;
                    self.current_instr.op_sz = !self.default_size();
                },
                0x67 => {
                    self.current_instr.addr_sz_prefix = true;
                    self.current_instr.addr_sz = !self.default_size();
                },
                _ => { }
            }

            debug!("instr address size: {:?}", self.current_instr.addr_sz_prefix);
            debug!("operand size prefix: {:?}", self.current_instr.op_sz_prefix);
        },
        consumed if self.instr_byte == 0x66 || self.instr_byte == 0x67];


        // Segment Override Prefix
        instr_state![self, Fetch3 -> Fetch4, {
            self.current_instr.seg_override_prefix = match self.instr_byte {
                0x2E => SegmentOverridePrefix::CS,
                0x36 => SegmentOverridePrefix::SS,
                0x3E => SegmentOverridePrefix::DS,
                0x26 => SegmentOverridePrefix::ES,
                0x64 => SegmentOverridePrefix::FS,
                0x65 => SegmentOverridePrefix::GS,
                _ => SegmentOverridePrefix::None
            };

            debug!("segment override prefix: {:?}", self.current_instr.seg_override_prefix);
        },
        consumed if self.current_instr.seg_override_prefix != SegmentOverridePrefix::None];


        // First Byte of Opcode
        instr_state![self, Fetch4 -> Fetch5, {
            self.current_instr.opcode = self.instr_byte as u16;

            if self.current_instr.opcode != 0x0F {
                debug!("1-byte opcode {:02x}", self.current_instr.opcode);

                self.current_instr.unresolved_operands = SINGLE_OPCODE_MAP[self.current_instr.opcode as usize];
                match self.current_instr.unresolved_operands {
                    UnresolvedOperands::NotImplemented => { panic!("Opcode {:02x} not implemented", self.current_instr.opcode); },
                    UnresolvedOperands::Invalid => { panic!("Invalid opcode {:02x}", self.current_instr.opcode); },
                    _ => { }
                };
                debug!("opcode operands: {:?}", self.current_instr.unresolved_operands);
            }
        },
        consumed if true,
        -> Fetch6 if self.current_instr.opcode != 0x0F];


        // Second Byte of Opcode
        instr_state![self, Fetch5 -> Fetch6, {
            self.current_instr.opcode = (self.current_instr.opcode << 8) | (self.instr_byte as u16);
            debug!("opcode second byte: {:02x}", self.instr_byte);

            self.current_instr.unresolved_operands = DOUBLE_OPCODE_MAP[self.instr_byte as usize];
            match self.current_instr.unresolved_operands {
                UnresolvedOperands::NotImplemented => { panic!("Opcode {:02x} not implemented", self.current_instr.opcode); },
                UnresolvedOperands::Invalid => { panic!("Invalid opcode {:02x}", self.current_instr.opcode); },
                _ => { }
            };
            debug!("opcode operands: {:?}", self.current_instr.unresolved_operands);
        },
        consumed if true];


        // ModR/M Byte
        instr_state![self, Fetch6 -> Fetch7, {
            if self.current_instr.unresolved_operands.has_modrm() {
                trace!("modrm: {:02x}", self.instr_byte);
                self.current_instr.modrm = Some(self.instr_byte);
                trace!("mod {:02b} regop {:03b} rm {:03b}",
                       self.current_instr.modrm_mod().unwrap(),
                       self.current_instr.modrm_regop().unwrap(),
                       self.current_instr.modrm_rm().unwrap());
            }
        },
        consumed if self.current_instr.modrm.is_some()];


        // SIB Byte
        instr_state![self, Fetch7 -> Fetch8, {
            self.current_instr.resolve();
            trace!("disp: {:08}({:?}) imm: {:08x}({:?})",
                self.current_instr.disp,
                self.current_instr.disp_sz,
                self.current_instr.imm,
                self.current_instr.imm_sz);
        },
        consumed if self.current_instr.has_sib()];


        // Displacement Byte 1
        instr_state![self, Fetch8 -> Fetch9, {
            if self.current_instr.disp_sz > 0 {
                // self.current_instr.disp = (self.instr_byte as u32) << 8 * (self.current_instr.disp_sz - 1);
                self.current_instr.disp = self.instr_byte as u32;
            }
        },
        consumed if self.current_instr.disp_sz > 0,
        -> Fetch12 if self.current_instr.disp_sz <= 1];


        // Displacement Byte 2
        instr_state![self, Fetch9 -> Fetch10, {
            // self.current_instr.disp |= (self.instr_byte as u32) << 8 * (self.current_instr.disp_sz - 2);
            self.current_instr.disp |= (self.instr_byte as u32) << 8;
        },
        consumed if true,
        -> Fetch12 if self.current_instr.disp_sz == 2];


        // Displacement Byte 3
        instr_state![self, Fetch10 -> Fetch11, {
            // self.current_instr.disp |= (self.instr_byte as u32) << 8 * (self.current_instr.disp_sz - 3);
            self.current_instr.disp |= (self.instr_byte as u32) << 16;
        },
        consumed if true,
        -> Fetch12 if self.current_instr.disp_sz == 3];


        // Displacement Byte 4
        instr_state![self, Fetch11 -> Fetch12, {
            // self.current_instr.disp |= (self.instr_byte as u32) << 8 * (self.current_instr.disp_sz - 4);
            self.current_instr.disp |= (self.instr_byte as u32) << 24;
        },
        consumed if true];


        // Immediate Byte 1
        instr_state![self, Fetch12 -> Fetch13, {
            if self.current_instr.imm_sz > 0 {
                self.current_instr.imm = self.instr_byte as u32;
            }
        },
        consumed if self.current_instr.imm_sz > 0,
        -> Decode if self.current_instr.imm_sz <= 1];


        // Immediate Byte 2
        instr_state![self, Fetch13 -> Fetch14, {
            self.current_instr.imm |= (self.instr_byte as u32) << 8;
        },
        consumed if true,
        -> Decode if self.current_instr.imm_sz == 2];


        // Immediate Byte 3
        instr_state![self, Fetch14 -> Fetch15, {
            self.current_instr.imm |= (self.instr_byte as u32) << 16;
        },
        consumed if true,
        -> Decode if self.current_instr.imm_sz == 3];


        // Immediate Byte 4
        instr_state![self, Fetch15 -> Decode, {
            self.current_instr.imm |= (self.instr_byte as u32) << 24;
        },
        consumed if true];

        instr_state![self, Decode -> Dispatch, {
            debug!("{:?}", self.current_instr);
            self.eip += self.instr_len - 1;
        }];

        instr_state![self, Dispatch -> HALT, {

        },
        -> GEN_OP_Read if self.current_instr.opcode & 0xFFC7 <= 0x0005,
        -> INC         if self.current_instr.opcode & 0xFFF8 == 0x0040,
        -> DEC         if self.current_instr.opcode & 0xFFF8 == 0x0048,
        -> PUSH        if self.current_instr.opcode & 0xFFF8 == 0x0050,
        -> PUSH        if self.current_instr.opcode          == 0x0068,
        -> PUSH        if self.current_instr.opcode          == 0x006A,
        -> POP         if self.current_instr.opcode & 0xFFF8 == 0x0058,
        -> INS_6x      if self.current_instr.opcode & 0xFFFE == 0x006C,
        -> MOV         if self.current_instr.opcode & 0xFFFC == 0x0088,
        -> MOV         if self.current_instr.opcode & 0xFFFD == 0x008C,
        -> MOV         if self.current_instr.opcode & 0xFFF0 == 0x00B0,
        -> MOV         if self.current_instr.opcode & 0xFFFE == 0x00C6,
        -> MOV         if self.current_instr.opcode & 0xFFFC == 0x00A0,
        -> TEST_Ax     if self.current_instr.opcode & 0xFFFE == 0x00A8,
        -> CALL_E8     if self.current_instr.opcode          == 0x00E8,
        -> JMP_EA      if self.current_instr.opcode          == 0x00EA,
        -> JMP_EB      if self.current_instr.opcode          == 0x00EB,
        -> IN_0        if self.current_instr.opcode & 0xFFF6 == 0x00E4,
        -> OUT_0       if self.current_instr.opcode & 0xFFF6 == 0x00E6,
        -> MOVZX_0FBx  if self.current_instr.opcode & 0xFFBE == 0x0FB6,
        -> MOV_0F2x    if self.current_instr.opcode & 0xFFF0 == 0x0F20,
        -> GRP1        if self.current_instr.opcode & 0xFFFC == 0x0080,
        -> GRP2        if self.current_instr.opcode & 0xFFFE == 0x00C0,
        -> GRP2        if self.current_instr.opcode & 0xFFFC == 0x00D0,
        -> Jcc_7x      if self.current_instr.opcode & 0xFFF0 == 0x0070,
        -> LEA         if self.current_instr.opcode          == 0x008D,
        -> RET_C3      if self.current_instr.opcode          == 0x00C3,
        -> CLC_F8      if self.current_instr.opcode          == 0x00F8,
        -> STC_F9      if self.current_instr.opcode          == 0x00F9,
        -> CLI_FA      if self.current_instr.opcode          == 0x00FA,
        -> STI_FB      if self.current_instr.opcode          == 0x00FB,
        -> CLD_FC      if self.current_instr.opcode          == 0x00FC,
        -> STD_FD      if self.current_instr.opcode          == 0x00FD,
        -> GRP5        if self.current_instr.opcode          == 0x00FF,
        -> GRP6        if self.current_instr.opcode          == 0x0F00,
        -> GRP7        if self.current_instr.opcode          == 0x0F01
        ];

        instr_state![self, GRP1 -> HALT, {
            match self.current_instr.operands {
                Operands::Double(op1, op2) => {
                    match self.read_op(op1) {
                        Some(op1val) => match self.read_op(op2) {
                            Some(op2val) => {
                                self.op1val = Some(op1val);
                                self.op2val = Some(op2val);
                            },
                            None => return
                        },
                        None => return
                    }
                },
                _ => panic!("unrecognized grp1 op operand")
            };

            trace!("ops {:?} {:?}", self.op1val, self.op2val);
        },
        -> ADD_0 if self.current_instr.modrm_regop().unwrap() == 0b000,
        -> OR_0  if self.current_instr.modrm_regop().unwrap() == 0b001,
        -> ADC_1 if self.current_instr.modrm_regop().unwrap() == 0b010,
        -> SBB_1 if self.current_instr.modrm_regop().unwrap() == 0b011,
        -> AND_2 if self.current_instr.modrm_regop().unwrap() == 0b100,
        -> SUB_2 if self.current_instr.modrm_regop().unwrap() == 0b101,
        -> XOR_3 if self.current_instr.modrm_regop().unwrap() == 0b110,
        -> CMP_3 if self.current_instr.modrm_regop().unwrap() == 0b111
        ];

        instr_state![self, GRP7 -> HALT, {
            // SGDT
            // SIDT
            // LGDT
            // LIDT
            // SMSW
            // LMSW
        },
        -> SGDT if self.current_instr.modrm_regop().unwrap() == 0b000,
        -> SIDT if self.current_instr.modrm_regop().unwrap() == 0b001,
        -> LGDT if self.current_instr.modrm_regop().unwrap() == 0b010,
        -> LIDT if self.current_instr.modrm_regop().unwrap() == 0b011,
        -> SMSW if self.current_instr.modrm_regop().unwrap() == 0b100,
        -> LMSW if self.current_instr.modrm_regop().unwrap() == 0b110
        ];

        instr_state![self, SGDT -> Post, {
            unimplemented!();
        }];

        instr_state![self, SIDT -> Post, {
            unimplemented!();
        }];

        instr_state![self, LGDT -> Post, {
            // NOP
        }];

        instr_state![self, LIDT -> Post, {
            unimplemented!();
        }];

        instr_state![self, SMSW -> Post, {
            unimplemented!();
        }];

        instr_state![self, LMSW -> Post, {
            unimplemented!();
        }];

        instr_state![self, GRP5 -> HALT, {

        },
        // -> INC  if self.current_instr.modrm_regop().unwrap() == 0b000,
        // -> DEC  if self.current_instr.modrm_regop().unwrap() == 0b001,
        -> CALL_FF2 if self.current_instr.modrm_regop().unwrap() == 0b010,
        // -> CALL if self.current_instr.modrm_regop().unwrap() == 0b011,
        // -> JMP  if self.current_instr.modrm_regop().unwrap() == 0b100,
        // -> JMP  if self.current_instr.modrm_regop().unwrap() == 0b101,
        -> PUSH if self.current_instr.modrm_regop().unwrap() == 0b110
        ];

        // 00|010|101

        instr_state![self, GEN_OP_Read -> HALT, {
            match self.current_instr.operands {
                Operands::Double(op1, op2) => {
                    match self.read_op(op1) {
                        Some(op1val) => match self.read_op(op2) {
                            Some(op2val) => {
                                self.op1val = Some(op1val);
                                self.op2val = Some(op2val);
                            },
                            None => return
                        },
                        None => return
                    }
                },
                _ => panic!("unrecognized gen op operand")
            };

            trace!("ops {:?} {:?}", self.op1val, self.op2val);
        },
        -> ADD_0 if self.current_instr.opcode & 0x38 == 0x00,
        -> ADC_1 if self.current_instr.opcode & 0x38 == 0x10,
        -> AND_2 if self.current_instr.opcode & 0x38 == 0x20,
        -> XOR_3 if self.current_instr.opcode & 0x38 == 0x30,
        -> OR_0  if self.current_instr.opcode & 0x38 == 0x08,
        -> SBB_1 if self.current_instr.opcode & 0x38 == 0x18,
        -> SUB_2 if self.current_instr.opcode & 0x38 == 0x28,
        -> CMP_3 if self.current_instr.opcode & 0x38 == 0x38
        ];

        instr_state![self, ADD_0 -> GEN_OP_Write, {
            let op1 = self.op1val.unwrap().to_unsigned();
            let op2 = self.op2val.unwrap()._as(op1.1);
            let (res, cf) = op1.overflowing_add(op2);
            let of = op1.is_sign_bit_set() == op2.is_sign_bit_set()
                && op2.is_sign_bit_set() != res.is_sign_bit_set();
            self.result = Some(res);
            self.update_flags(res, Some(cf), Some(false), Some(of));
        }];

        instr_state![self, ADC_1 -> GEN_OP_Write, {
            unimplemented!();
        }];

        instr_state![self, AND_2 -> GEN_OP_Write, {
            let op1 = self.op1val.unwrap();
            let op2 = self.op2val.unwrap()._as(op1.1);
            let res = op1 & op2;
            self.result = Some(res);
            self.update_flags(res, Some(false), None, Some(false));
        }];

        instr_state![self, XOR_3 -> GEN_OP_Write, {
            let op1 = self.op1val.unwrap();
            let op2 = self.op2val.unwrap()._as(op1.1);
            let res = op1 ^ op2;
            self.result = Some(res);
            self.update_flags(res, Some(false), None, Some(false));
        }];

        instr_state![self, OR_0 -> GEN_OP_Write, {
            let op1 = self.op1val.unwrap();
            let op2 = self.op2val.unwrap()._as(op1.1);
            let res = op1 | op2;
            self.result = Some(res);
            self.update_flags(res, Some(false), None, Some(false));
        }];

        instr_state![self, SBB_1 -> GEN_OP_Write, {
            unimplemented!();
        }];

        instr_state![self, SUB_2 -> GEN_OP_Write, {
            let op1 = self.op1val.unwrap().to_unsigned();
            let op2 = self.op2val.unwrap().to_signed()._as(op1.1).to_unsigned();
            let (val, cf) = op1.overflowing_sub(op2);
            let of = op1.is_sign_bit_set() != op2.is_sign_bit_set()
                && op2.is_sign_bit_set() == val.is_sign_bit_set();
            self.result = Some(val);
            self.update_flags(val, Some(cf), Some(false), Some(of));
        }];

        instr_state![self, CMP_3 -> Post, {
            let op1 = self.op1val.unwrap().to_unsigned();
            let op2 = self.op2val.unwrap().to_signed()._as(op1.1).to_unsigned();
            trace!("COMPARE {:?} and {:?}", op1, op2);
            let (val, cf) = op1.overflowing_sub(op2);
            let of = op1.is_sign_bit_set() != op2.is_sign_bit_set()
                && op2.is_sign_bit_set() == val.is_sign_bit_set();
            trace!("sign bits op1 {} op2 {} val {} val {:?} of {} cf {}",
                   op1.is_sign_bit_set(), op2.is_sign_bit_set(), val.is_sign_bit_set(),
                   val, of, cf);
            self.update_flags(val, Some(cf), Some(false), Some(of));
        }];

        instr_state![self, INC -> Post, {
            let reg = match self.current_instr.operands {
                Operands::Single(Op::Register(reg)) => reg,
                _ => panic!("unexpected operands for inc")
            };

            let val = self.gen_regs.read(reg);
            let (res, cf) = val.overflowing_add(Num(1, val.1));
            self.gen_regs.write(reg, res.to_u32().unwrap());
            self.update_flags(val, Some(cf), Some(false), None);
        }];

        instr_state![self, DEC -> Post, {
            let reg = match self.current_instr.operands {
                Operands::Single(Op::Register(reg)) => reg,
                _ => panic!("unexpected operands for inc")
            };

            let val = self.gen_regs.read(reg);
            let (res, cf) = val.overflowing_sub(Num(1, val.1));
            self.gen_regs.write(reg, res.to_u32().unwrap());
            self.update_flags(val, Some(cf), Some(false), None);
        }];

        instr_state![self, GRP2 -> HALT, {
            // ROL ROR RCL RCR SHL SHR     SAR
            match self.current_instr.operands {
                Operands::Double(op1, op2) => {
                    match self.read_op(op1) {
                        Some(op1val) => match self.read_op(op2) {
                            Some(op2val) => {
                                self.op1val = Some(op1val);
                                self.op2val = Some(op2val);
                            },
                            None => return
                        },
                        None => return
                    }
                },
                _ => panic!("unrecognized group 2 operand")
            };
        },
        -> ROL if self.current_instr.modrm_regop().unwrap() == 0b000,
        -> ROR if self.current_instr.modrm_regop().unwrap() == 0b001,
        -> RCL if self.current_instr.modrm_regop().unwrap() == 0b010,
        -> RCR if self.current_instr.modrm_regop().unwrap() == 0b011,
        -> SHL if self.current_instr.modrm_regop().unwrap() == 0b100,
        -> SHR if self.current_instr.modrm_regop().unwrap() == 0b101,
        -> SAR if self.current_instr.modrm_regop().unwrap() == 0b111
        ];

        instr_state![self, ROL -> Post, {
            unimplemented!();
        }];

        instr_state![self, ROR -> Post, {
            unimplemented!();
        }];

        instr_state![self, RCL -> Post, {
            unimplemented!();
        }];

        instr_state![self, RCR -> Post, {
            unimplemented!();
        }];

        instr_state![self, SHL -> Post, {
            let mut rm = self.op1val.unwrap().to_unsigned();
            let mut count =
                self.op2val.unwrap().to_unsigned().to_u32().unwrap() & 0x1F;
            while count > 0 {
                self.update_flag(::reg::EFLAGS_CF, rm.is_sign_bit_set());
                rm = rm << 1;
                count -= 1;
            }
            
            self.result = Some(rm);
        }];

        instr_state![self, SHR -> GEN_OP_Write, {
            let mut rm = self.op1val.unwrap().to_unsigned();
            let mut count =
                self.op2val.unwrap().to_unsigned().to_u32().unwrap() & 0x1F;
            while count > 0 {
                self.update_flag(::reg::EFLAGS_CF, rm.is_low_bit_set());
                rm = rm >> 1;
                count -= 1;
            }
            
            self.result = Some(rm);
        }];

        instr_state![self, SAR -> Post, {
            unimplemented!();
        }];

        instr_state![self, GEN_OP_Write -> Post, {
            match self.current_instr.operands {
                Operands::Double(op, _) => {
                    let res = self.result.unwrap();
                    match self.write_op(op, res) {
                        Some(()) => { },
                        None => return
                    }
                },
                _ => panic!("unrecognized gen op operand")
            }
        }];

        instr_state![self, PUSH -> Post, {
            let op = match self.current_instr.operands {
                Operands::Single(uop) => match self.read_op(uop) {
                    Some(op) => op,
                    None => return
                },
                _ => panic!("unrecognized push operand")
            };

            let res = self.push(op);
            if !res.is_some() { return; }
        }];

        instr_state![self, POP -> Post, {
            let reg = match self.current_instr.operands {
                Operands::Single(Op::Register(reg)) => reg,
                _ => panic!("unrecognized push operand")
            };

            let res = self.pop_sz(reg.size());
            if !res.is_some() { return; }
            self.gen_regs.write(reg, res.unwrap().to_u32().unwrap());
        }];

        instr_state![self, Jcc_7x -> Post, {
            let off = Num(self.current_instr.imm,
                          Size::from_u32(self.current_instr.imm_sz).unwrap().signed());
            trace!("JUMP OFF {:?}", off);
            let cond = match self.current_instr.opcode & 0x0F {
                0x0 =>  self.eflags.contains(  ::reg::EFLAGS_OF),
                0x1 => !self.eflags.contains(  ::reg::EFLAGS_OF),
                0x2 =>  self.eflags.contains(  ::reg::EFLAGS_CF),
                0x3 => !self.eflags.contains(  ::reg::EFLAGS_CF),
                0x4 =>  self.eflags.contains(  ::reg::EFLAGS_ZF),
                0x5 => !self.eflags.contains(  ::reg::EFLAGS_ZF),
                0x6 =>  self.eflags.contains(  ::reg::EFLAGS_CF | ::reg::EFLAGS_ZF),
                0x7 => !self.eflags.intersects(::reg::EFLAGS_CF | ::reg::EFLAGS_ZF),
                0x8 =>  self.eflags.contains(  ::reg::EFLAGS_SF),
                0x9 => !self.eflags.contains(  ::reg::EFLAGS_SF),
                0xA =>  self.eflags.contains(  ::reg::EFLAGS_PF),
                0xB => !self.eflags.contains(  ::reg::EFLAGS_PF),
                0xC =>  self.eflags.contains(  ::reg::EFLAGS_SF) != self.eflags.contains(::reg::EFLAGS_OF),
                0xD =>  self.eflags.contains(  ::reg::EFLAGS_SF) == self.eflags.contains(::reg::EFLAGS_OF),
                0xE =>  self.eflags.contains(  ::reg::EFLAGS_SF) != self.eflags.contains(::reg::EFLAGS_OF)
                    &&  self.eflags.contains(  ::reg::EFLAGS_ZF),
                0xF =>  self.eflags.contains(  ::reg::EFLAGS_SF) == self.eflags.contains(::reg::EFLAGS_OF)
                    && !self.eflags.contains(  ::reg::EFLAGS_ZF),
                _ => panic!("unknown jump")
            };

            if cond {
                let target = ((self.eip as i32) + off._as(Type::i32).to_i32().unwrap()) as u32;
                trace!("JUMP {:?} TO {:?}", off._as(Type::i32).to_i32(), target);
                self.eip = target;
            }
        }];

        instr_state![self, LEA -> Post, {
            let (reg, addr) = match self.current_instr.operands {
                Operands::Double(Op::Register(reg),
                    Op::MemoryAddress(_, base, index, scale, disp, sz)) => {
                    (reg, self.calc_address(base, index, scale, disp))
                },
                _ => panic!("unexpected lea operands")
            };

            match reg.size() {
                Size::Size16 => {
                    match addr.size() {
                        Size::Size16 => {
                            self.gen_regs.write(reg, addr.to_u16().unwrap() as u32);
                        },
                        Size::Size32 => {
                            self.gen_regs.write(reg, addr.to_u32().unwrap() & 0x0000FFFF);
                        },
                        _ => panic!("unexpectedd address size for lea")
                    };
                },
                Size::Size32 => {
                    match addr.size() {
                        Size::Size16 => {
                            self.gen_regs.write(reg, addr.to_u32().unwrap() & 0x0000FFFF);
                        },
                        Size::Size32 => {
                            self.gen_regs.write(reg, addr.to_u32().unwrap());
                        },
                        _ => panic!("unexpected address size for lea")
                    };
                },
                _ => panic!("unexpected reg size for lea")
            }

        }];

        instr_state![self, TEST_Ax -> Post, {
            match self.current_instr.operands {
                Operands::Double(uop1, uop2) => {
                    let op1 = self.read_op(uop1);
                    if !op1.is_some() { return; }
                    let op2 = self.read_op(uop2);
                    if !op2.is_some() { return; }

                    trace!("{:?} {:?}", op1.unwrap().size(), op2.unwrap().size());
                    let res = op1.unwrap().to_unsigned() & op2.unwrap().to_unsigned();
                    self.update_flags(res, Some(false), None, Some(false));
                },
                _ => panic!("unexpected operand")
            }
        }];

        instr_state![self, MOV -> Post, {
            match self.current_instr.operands {
                Operands::Double(op1, op2) => {
                    let op = self.read_op(op2);
                    if !op.is_some() { return; }
                    let res = self.write_op(op1, op.unwrap());
                    if !res.is_some() { return; }
                },
                _ => panic!("unexpected operand")
            }
        }];

        instr_state![self, INS_6x -> Post, {
            let (dst_addr, sz, di_reg, src_port) = match self.current_instr.operands {
                Operands::Double(mem, Op::Register(reg)) => {
                    match mem {
                        Op::MemoryAddress(_, Some(di), _, _, _, sz) => {
                            (mem, sz, di, self.gen_regs.read(reg).0 as u16)
                        },
                        _ => panic!("unexpected operands for INS_6x")
                    }
                },
                _ => panic!("unexpected operands for INS_6x")
            };

            trace!("INS READ SIZE {:?}", sz);
            let res = self.read_io(src_port, sz);
            if !res.is_some() { return; }

            let memres = self.write_op(dst_addr, res.unwrap());
            if !memres.is_some() { return; }
            self.io_read_result = None;

            let inc = (sz.to_i32().unwrap()
                * if self.eflags.contains(::reg::EFLAGS_DF) { -1 } else { 1 }) as u32;
            let di = self.gen_regs.read(di_reg).to_u32().unwrap();
            self.gen_regs.write(di_reg, (di + inc) & sz.mask());
        }];

        instr_state![self, IN_0 -> Post, {
            let (dst_reg, src_port) = match self.current_instr.operands {
                Operands::Double(Op::Register(reg), Op::Immediate(imm)) =>
                    (reg, self.current_instr.imm as u16),
                Operands::Double(Op::Register(reg1), Op::Register(reg2)) =>
                    (reg1, self.gen_regs.read(reg2).0 as u16),
                _ => panic!("unexpected operands for IN_0")
            };

            let res = self.read_io(src_port, dst_reg.size());
            if !res.is_some() { return; }
            self.io_read_result = None;

            trace!("write io {:x} to reg {:?}", res.unwrap(), dst_reg);
            self.gen_regs.write(dst_reg, res.unwrap().to_u32().unwrap());
        }];

        instr_state![self, OUT_0 -> Post, {
            let (dst_port, src_reg) = match self.current_instr.operands {
                Operands::Double(Op::Immediate(imm), Op::Register(reg)) =>
                    (self.current_instr.imm as u16, reg),
                Operands::Double(Op::Register(reg1), Op::Register(reg2)) =>
                    (self.gen_regs.read(reg1).0 as u16, reg2),
                _ => panic!("unexpected operands for OUT_0")
            };

            let val = self.gen_regs.read(src_reg);
            let res = self.write_io(dst_port, val);
            if !res.is_some() { return; }
            self.io_read_result = None;
        }];

        instr_state![self, RET_C3 -> Post, {
            let res = self.pop();
            if !res.is_some() { return; }
            let new_ip = match self.current_instr.addr_sz {
                Size::Size16 => res.unwrap() & 0x0000FFFF,
                Size::Size32 => res.unwrap(),
                _ => panic!("invalid addr sz in ret")
            };

            self.eip = new_ip.to_u32().unwrap();
        }];

        instr_state![self, CALL_E8 -> Post, {
            let off = Num(self.current_instr.disp,
                          Size::from_u32(self.current_instr.disp_sz).unwrap().unsigned());
            let num = Num(self.eip, self.current_instr.addr_sz.unsigned());
            let res = self.push(num);
            if !res.is_some() { return; }

            let new_eip = (self.eip as i32 + off.to_u32().unwrap() as i32) as u32;
            trace!("CALL FROM EIP {:08x} TO EIP {:08x}", self.eip, new_eip);
            self.eip = new_eip;
        }];

        instr_state![self, CALL_FF2 -> Post, {
            let addr = match self.current_instr.operands {
                Operands::Single(uop) =>
                    match self.read_op(uop) {
                        Some(addr) => addr,
                        None => return
                    },
                _ => panic!("unexpected operand to call ff2")
            };

            let num = Num(self.eip, self.current_instr.addr_sz.unsigned());
            let res = self.push(num);
            if !res.is_some() { return; }

            self.eip = addr.to_u32().unwrap();
        }];

        instr_state![self, JMP_EA -> Post, {
            match self.mode() {
                ProcessorMode::Real => {
                    trace!("INSTR DISP: {:08x}", self.current_instr.disp);
                    self.has_ljmped = true;
                    trace!("jmp disp: {:08x} imm: {:08x}", self.current_instr.disp, self.current_instr.imm);
                    if self.current_instr.op_sz == Size::Size16 {
                        self.seg_regs.write(SegmentRegister::CS, self.current_instr.imm);
                        self.eip = self.current_instr.disp & 0x0000FFFF;
                    } else {
                        self.seg_regs.write(SegmentRegister::CS, self.current_instr.imm);
                        self.eip = self.current_instr.disp;
                    }
                },
                ProcessorMode::Protected => {
                    trace!("jmp disp: {:08x} imm: {:08x}", self.current_instr.disp, self.current_instr.imm);
                    // ignore segments
                    self.eip = self.current_instr.disp & 0x0000FFFF;
                }
            }
        }];

        instr_state![self, JMP_EB -> Post, {
            let off = match self.current_instr.operands {
                Operands::Single(uop) =>
                    match self.read_op(uop) {
                        Some(op) => op,
                        None => return
                    },
                _ => panic!("unexpected jmp eb operands")
            };

            trace!("JMP EB OFF {:?}", off);
            self.eip = (self.eip as i32 + off._as(Type::u32).to_u32().unwrap() as i32) as u32;
        }];

        instr_state![self, CLC_F8 -> Post, {
            self.eflags.remove(::reg::EFLAGS_CF);
        }];

        instr_state![self, STC_F9 -> Post, {
            self.eflags.insert(::reg::EFLAGS_CF);
        }];

        instr_state![self, CLI_FA -> Post, {
            self.eflags.remove(::reg::EFLAGS_IF);
        }];

        instr_state![self, STI_FB -> Post, {
            self.eflags.insert(::reg::EFLAGS_IF);
        }];

        instr_state![self, CLD_FC -> Post, {
            self.eflags.remove(::reg::EFLAGS_DF);
        }];

        instr_state![self, STD_FD -> Post, {
            self.eflags.insert(::reg::EFLAGS_DF);
        }];

        instr_state![self, MOV_0F2x -> Post, {
            match self.current_instr.operands  {
                Operands::Double(op1, op2) =>
                    match self.read_op(op2) {
                        Some(val) => {
                            self.write_op(op1, val);
                        },
                        None => return
                    },
                _ => panic!("unexpected operands for MOV")
            }
        }];

        instr_state![self, MOVZX_0FBx -> Post, {
            match self.current_instr.operands  {
                Operands::Double(op1, op2) =>
                    match self.read_op(op2) {
                        Some(val) => {
                            self.write_op(op1, val);
                        },
                        None => return
                    },
                _ => panic!("unexpected operands for MOVZX")
            }
        }];

        instr_state![self, HALT -> HALT, {
            trace!("halt");
        }];

        instr_state![self, Post -> Fetch0, {
            trace!("POST");
            let pre = self.current_instr.instr_prefix;
            if pre == InstructionPrefix::REP || pre == InstructionPrefix::REPN {
                // eCX
                let sz = self.current_instr.addr_sz;
                let reg = Register::decode(0b001, sz);
                let mut val = self.gen_regs.read(reg).to_u32().unwrap();;
                val = val - 1;
                self.gen_regs.write(reg, val);

                let op = self.current_instr.opcode;
                let exit = (op == 0xA6 /* CMPSB */ || op == 0xA7 /* CMPSW */
                    || op == 0xAE /* SCASB */ || op == 0xAF /* SCASW */)
                    && ((pre == InstructionPrefix::REP && self.eflags.contains(::reg::EFLAGS_ZF))
                        || (pre == InstructionPrefix::REPN && !self.eflags.contains(::reg::EFLAGS_ZF)));

                if !(val == 0 || exit) {
                    self.instr_state = InstructionState::Dispatch;
                    return;
                }
            }
        }];
    }

    fn bus_transfer_cycle(&mut self, state: &BusState) {
        match self.bus_transfer_state {
            BusTransferState::T2 => {
                trace!("GOT BUS READY {:?} W/R {:?} D/C {:?} M/IO {:?}",
                       state.read(BusLine::READY),
                       state.read(BusLine::W_R),
                       state.read(BusLine::D_C),
                       state.read(BusLine::M_IO));
                if state.read(BusLine::READY)  == Signal::Low {

                    self.bus_transfer_state = BusTransferState::Idle;
                    if state.read(BusLine::D_C) == Signal::High {
                        match state.read(BusLine::W_R) {
                            // read
                            Signal::Low =>
                                match state.read(BusLine::M_IO) {
                                    Signal::Low => {
                                        self.io_read_result = Some((
                                                state.read_address() as u16,
                                                Num(state.read_data(),
                                                    Size::from_u32(state.read_size() as u32).unwrap().unsigned())
                                                ))
                                    },
                                    Signal::High => {
                                        self.cache.write(
                                            state.read_address(),
                                            state.read_data()
                                            )
                                    },
                                    _ => panic!("m/io undefined")
                                },
                            // write
                            Signal::High =>
                                match state.read(BusLine::M_IO) {
                                    Signal::Low => {
                                        self.io_read_result = Some((
                                                state.read_address() as u16,
                                                Num(0,
                                                    Size::from_u32(state.read_size() as u32).unwrap().unsigned())
                                                ))
                                    },
                                    Signal::High => {
                                        self.cache.write(
                                            state.read_address(),
                                            state.read_data()
                                            )
                                    },
                                    _ => panic!("m/io undefined")
                                },
                            _ => panic!("w/r undefined")
                        }
                    }
                }
            },
            _ => {

            }
        }

    }

    // Utility

    fn mode(&self) -> ProcessorMode {
        let cr0 = self.con_regs.read(ControlRegister::CR0).to_u32().unwrap();
        trace!("cr0: {:08x}", cr0);
        let cr0f = CR0::from_bits(cr0).unwrap();
        if cr0f.contains(::reg::CR0_PE) {
            ProcessorMode::Protected
        } else {
            ProcessorMode::Real
        }
    }

    fn default_size(&self) -> Size {
        match self.mode() {
            ProcessorMode::Real      => Size::Size16,
            ProcessorMode::Protected => Size::Size32
        }
    }

    fn eip(&self) -> Num {
        // TODO: protected mode
        match self.mode() {
            ProcessorMode::Real      =>
                self.seg_addr(SegmentRegister::CS)._as(Type::u32) + self.eip
                    | if self.has_ljmped { 0 } else { 0xFFF00000 },
            ProcessorMode::Protected => Num(self.eip, Type::u32)
        }
    }

    fn read_op(&mut self, op: Op) -> Option<Num> {
        match op {
            Op::Register(reg) => Some(self.gen_regs.read(reg)),

            Op::SegmentRegister(reg) => Some(self.seg_regs.read(reg)),
            Op::ControlRegister(reg) => Some(self.con_regs.read(reg)),
            Op::DebugRegister(reg) => Some(self.dbg_regs.read(reg)),
            Op::TestRegister(reg) => Some(self.tst_regs.read(reg)),

            Op::FlagsRegister(sz) => Some(Num(self.eflags.bits(), sz.unsigned())),
            Op::MemoryAddress(seg_reg, base, index, scale, disp_sz, size) => {
                let addr = self.calc_address_seg(seg_reg, base, index, scale, disp_sz);
                self.mmu_read_mem_sz(addr.to_u32().unwrap(), size)
                    .map_or(None, |val| Some(Num(val, size.unsigned())))
            },
            Op::Immediate(sz) => Some(Num(self.current_instr.imm, sz.signed())),
            Op::RelativeAddress(sz) =>
                Some(Num(self.current_instr.imm, sz.signed())),
            Op::Constant(con) => Some(Num(con, Type::u32)),
            Op::Offset(sz) => Some(Num(self.current_instr.disp, sz.unsigned())),
            _ => panic!("unhandled read op")
        }
    }

    fn write_op(&mut self, op: Op, num: Num) -> Option<()> {
        trace!("WRITE NUM {:?}", num);
        match op {
            Op::Register(reg) => Some(self.gen_regs.write(reg, num.to_unsigned().to_u32().unwrap())),

            Op::SegmentRegister(sreg) => Some(self.seg_regs.write(sreg, num.to_unsigned().to_u32().unwrap())),
            Op::ControlRegister(reg) => Some(self.con_regs.write(reg, num.to_unsigned().to_u32().unwrap())),
            Op::DebugRegister(reg) => Some(self.dbg_regs.write(reg, num.to_unsigned().to_u32().unwrap())),
            Op::TestRegister(reg) => Some(self.tst_regs.write(reg, num.to_unsigned().to_u32().unwrap())),

            Op::MemoryAddress(seg_reg, base, index, scale, disp_sz, size) => {
                let addr = self.calc_address_seg(seg_reg, base, index, scale, disp_sz);
                self.mmu_write_mem(addr.to_u32().unwrap(), num)
            },
            _ => panic!("unhandled write op")
        }
    }

    fn calc_address_seg(&mut self, seg_reg: SegmentRegister, base: Option<Register>,
        index: Option<Register>, scale: u8, disp_sz: Option<Size>) -> Num {
        let sz = self.current_instr.addr_sz.signed();
        let z = Num(0, sz);

        let s = self.seg_addr(seg_reg)._as(sz);
        let b =  base.map_or(z, |reg| self.gen_regs.read(reg))._as(sz);
        let i = index.map_or(z, |reg| self.gen_regs.read(reg))._as(sz) << (scale as usize);
        let d = disp_sz.map_or(z, |s| Num(self.current_instr.disp, s.signed()))._as(sz);

        (s + b + i + d).to_unsigned()
        
        // (self.seg_addr(seg_reg)._as(sz)
        //     + base.map_or(z, |reg| self.gen_regs.read(reg).to_signed())
        //     + index.map_or(z, |reg| self.gen_regs.read(reg).to_signed() << (scale as usize))
        //     + disp_sz.map_or(z,
        //         |sz| Num(self.current_instr.disp, sz.signed()))).to_unsigned()
    }

    fn calc_address(&mut self, base: Option<Register>,
        index: Option<Register>, scale: u8, disp_sz: Option<Size>) -> Num {
        let sz = self.current_instr.addr_sz.signed();
        let z = Num(0, sz);

        let b =  base.map_or(z, |reg| self.gen_regs.read(reg))._as(sz);
        let i = index.map_or(z, |reg| self.gen_regs.read(reg))._as(sz) << (scale as usize);
        let d = disp_sz.map_or(z, |s| Num(self.current_instr.disp, s.signed()))._as(sz);

        (b + i + d).to_unsigned()
        
        // (base.map_or(z, |reg| self.gen_regs.read(reg)._as(sz))
        //     + index.map_or(z, |reg| self.gen_regs.read(reg)._as(sz) << (scale as usize))
        //     + disp_sz.map_or(z,
        //         |sz| Num(self.current_instr.disp, sz.signed()))._as(sz)).to_unsigned()
    }

    fn seg_addr(&self, seg_reg: SegmentRegister) -> Num {
        match self.mode() {
            ProcessorMode::Real => self.seg_regs.read(seg_reg)._as(Type::u32) << 4,
            ProcessorMode::Protected => Num(0, Type::u32)
        }
    }

    fn update_flags(&mut self, num: Num,
                    cf: Option<bool>, af: Option<bool>, of: Option<bool>) {
        if cf.is_some() {
            self.update_flag(::reg::EFLAGS_CF, cf.unwrap());
        }
        self.update_flag(::reg::EFLAGS_PF, num.low_byte_parity());
        if af.is_some() {
            // decimal arith, not needed (?)
            self.update_flag(::reg::EFLAGS_AF, af.unwrap());
        }
        self.update_flag(::reg::EFLAGS_ZF, num == 0);
        self.update_flag(::reg::EFLAGS_SF, num < 0);
        if of.is_some() {
            self.update_flag(::reg::EFLAGS_OF, of.unwrap());
        }
    }

    fn update_flag(&mut self, flag: EFlags, pred: bool) {
        if pred {
            self.eflags.insert(flag);
        } else {
            self.eflags.remove(flag);
        }
    }

    fn print_state(&self) {
        debug!("eip {:08x}    eflags {:?}", self.eip, self.eflags);
        debug!("eax {:08x}    ecx {:08x}    esp {:08x}    esi {:08x}",
               self.gen_regs.read(Register::EAX),
               self.gen_regs.read(Register::ECX),
               self.gen_regs.read(Register::ESP),
               self.gen_regs.read(Register::ESI));
        debug!("ebx {:08x}    edx {:08x}    ebp {:08x}    edi {:08x}",
               self.gen_regs.read(Register::EBX),
               self.gen_regs.read(Register::EDX),
               self.gen_regs.read(Register::EBP),
               self.gen_regs.read(Register::EDI));
        debug!("cs {:04x} ds {:04x} fs {:04x}",
               self.seg_regs.read(SegmentRegister::CS),
               self.seg_regs.read(SegmentRegister::DS),
               self.seg_regs.read(SegmentRegister::FS));
        debug!("ss {:04x} es {:04x} gs {:04x}",
               self.seg_regs.read(SegmentRegister::SS),
               self.seg_regs.read(SegmentRegister::ES),
               self.seg_regs.read(SegmentRegister::GS));
    }

    fn pop(&mut self) -> Option<Num> {
        let size = self.current_instr.op_sz;
        self.pop_sz(size)
    }

    fn pop_sz(&mut self, size: Size) -> Option<Num> {
        let sp = Register::decode(0b100, self.current_instr.addr_sz);
        let spv = self.gen_regs.read(sp);
        let stack = (self.seg_addr(SegmentRegister::SS) + spv).to_u32().unwrap();

        let res = self.mmu_read_mem_sz(stack, size);
        if !res.is_some() { return None; }
        self.gen_regs.write(sp, (spv + size as u32).to_u32().unwrap());
        Some(Num(res.unwrap(), size.unsigned()))
    }

    fn push(&mut self, num: Num) -> Option<()> {
        let sp = Register::decode(0b100, self.current_instr.addr_sz);
        let spv = self.gen_regs.read(sp);
        let stack = (self.seg_addr(SegmentRegister::SS) + spv).to_u32().unwrap();
        let sz = num.size().to_u32().unwrap();

        let res = self.mmu_write_mem(stack - sz, num);
        if !res.is_some() { return None; }
        self.gen_regs.write(sp, (spv - sz).to_u32().unwrap());
        res
    }

    // MMU functions

    fn mmu_read_mem_sz(&mut self, vaddr: VAddr, size: Size) -> Option<u32> {
        let paddr = vaddr as PAddr;
        match size {
            Size::Size8 =>  self.mmu_read_mem::<u8 >(paddr).map(|x| x as u32),
            Size::Size16 => self.mmu_read_mem::<u16>(paddr).map(|x| x as u32),
            Size::Size32 => self.mmu_read_mem::<u32>(paddr),
            _ => panic!("unhandled read size")
        }
    }

    fn mmu_read_mem<T>(&mut self, paddr: PAddr) -> Option<T> where T: FromPrimitive {
        assert!(self.bus_transfer_state == BusTransferState::Idle);

        let count = mem::size_of::<T>() as u32;
        assert!(count == 1 || count == 2 || count == 4, "count must be in 1, 2, 4");

        let aligned_paddr = paddr & !0x3;
        let aligned_paddr_2 = (paddr + count - 1) & !0x3;

        if aligned_paddr != aligned_paddr_2 {
            let half_count = count >> 1;
            let sz = Size::from_u32(half_count).unwrap();
            if let Some(low) = self.mmu_read_mem_sz(paddr, sz) {
                if let Some(high) = self.mmu_read_mem_sz(paddr + half_count, sz) {
                    if let Some(converted) = T::from_u32((high << (half_count * 8)) | low) {
                        return Some(converted);
                    } else {
                        unreachable!();
                    }
                }
            }
        } else {
            if let Some(val) = self.mmu_read_mem_u32_aligned(aligned_paddr) {
                let off = (paddr % 4) * 8;
                let mask = 0xFFFFFFFFu32 >> (32 - count * 8);
                let masked_val = (val >> off) & mask;
                if let Some(converted) = T::from_u32(masked_val) {
                    return Some(converted);
                } else {
                    unreachable!();
                }
            }
        }

        None
    }

    fn mmu_read_mem_u32_aligned(&mut self, paddr: PAddr) -> Option<u32> {
        assert!(self.bus_transfer_state == BusTransferState::Idle);
        assert!(paddr % 4 == 0, "paddr not 4 byte aligned");
        debug!("load u32 {:08x}", paddr);

        // check rom(s)
        for &(ref range, ref rom) in self.roms.iter() {
            if range.start <= (paddr as u64) && (paddr as u64) < range.end {
                debug!("found in rom");
                return Some(rom.read(paddr - range.start as u32));
            }
        }

        if let Some(ref cga) = self.cga {
            if 0xB8000 <= paddr && paddr < 0xC0000 {
                // video ram
                return Some(cga.read_u32(paddr));
            }
        }

        // check cache
        let cached = self.cache.read(paddr);
        if cached.is_some() {
            debug!("found in cache");
            return cached;
        }

        debug!("not found; go to memory");

        // read in from memory
        self.bus_is_write = false;
        self.bus_is_data = true;
        self.bus_is_mem = true;
        self.bus_addr = paddr;
        self.bus_transfer_state = BusTransferState::T1;
        self.bus_data_size = 4;
        None
    }

    fn mmu_write_mem(&mut self, paddr: PAddr, num: Num) -> Option<()> {
        assert!(self.bus_transfer_state == BusTransferState::Idle);

        let count = num.size().to_u32().unwrap();
        assert!(count == 1 || count == 2 || count == 4, "count must be in 1, 2, 4");

        let aligned_paddr = paddr & !0x3;
        let aligned_paddr_2 = (paddr + count - 1) & !0x3;

        if aligned_paddr != aligned_paddr_2 {
            let half_count = count >> 1;
            let ty = Size::from_u32(half_count).unwrap().unsigned();
            let mask = 0xFFFFFFFF >> (32 - half_count * 8);
            let low = (num & mask)._as(ty);
            let high = (num >> (half_count * 8) as usize)._as(ty);

            if let Some(()) = self.mmu_write_mem(paddr, low) {
                if let Some(()) = self.mmu_write_mem(paddr + half_count, high) {
                    return Some(());
                }
            }
        } else {
            if let Some(val) = self.mmu_read_mem_u32_aligned(aligned_paddr) {
                let off = (paddr % 4) * 8;
                let mask = (0xFFFFFFFFu32 >> (32 - count * 8)) << off;
                
                let raw_num = (num.to_u32().unwrap() & num.size().mask()) << off;
                let new_val = (val & !mask) | (raw_num & mask);

                if let Some(_) = self.mmu_write_mem_u32_aligned(aligned_paddr, new_val) {
                    return Some(());
                }
            }
        }

        None
    }

    fn mmu_write_mem_u32_aligned(&mut self, paddr: PAddr, val: u32) -> Option<()> {
        assert!(self.bus_transfer_state == BusTransferState::Idle);
        assert!(paddr % 4 == 0, "paddr not 4 byte aligned");
        debug!("write u32 {:08x}", paddr);

        // check rom(s)
        for &(ref range, ref rom) in self.roms.iter() {
            if range.start <= (paddr as u64) && (paddr as u64) < range.end {
                panic!("tried to write to rom");
            }
        }

        if let Some(ref mut cga) = self.cga {
            if 0xB8000 <= paddr && paddr < 0xC0000 {
                // video ram
                cga.write_u32(paddr, val);
                return Some(());
            }
        }

        // check cache
        let cached = self.cache.read(paddr);
        if cached.is_some() && cached.unwrap() == val {
            debug!("found in cache");
            return Some(());
        }

        debug!("not found; go to memory");

        // read in from memory
        self.bus_is_write = true;
        self.bus_is_data = true;
        self.bus_is_mem = true;
        self.bus_addr = paddr;
        self.bus_transfer_state = BusTransferState::T1;
        self.bus_data = val;
        self.bus_data_size = 4;
        None
    }

    fn read_io(&mut self, addr: IOAddr, size: Size) -> Option<Num> {
        assert!(self.bus_transfer_state == BusTransferState::Idle);

        let count = size.to_u32().unwrap();
        assert!(count == 1 || count == 2 || count == 4, "count must be in 1, 2, 4");
        assert!((addr as u32) % count == 0, "io access must be aligned");

        if let Some((ioaddr, val)) = self.io_read_result {
            if addr == ioaddr {
                trace!("IO RESULT {:08x} {:08x} VAL {:08x}", addr, ioaddr, val);
                return Some(val)
            }
        }

        self.bus_is_write = false;
        self.bus_is_data = true;
        self.bus_is_mem = false;
        self.bus_addr = addr as u32;
        self.bus_transfer_state = BusTransferState::T1;
        self.bus_data_size = count as usize;
        None
    }

    fn write_io(&mut self, addr: IOAddr, num: Num) -> Option<()> {
        assert!(self.bus_transfer_state == BusTransferState::Idle);

        let count = num.size().to_u32().unwrap();
        assert!(count == 1 || count == 2 || count == 4, "count must be in 1, 2, 4");
        assert!((addr as u32) % count == 0, "io access must be aligned");

        if let Some((ioaddr, val)) = self.io_read_result {
            if addr == ioaddr {
                trace!("IO RESULT {:08x} {:08x} VAL {:08x}", addr, ioaddr, val);
                return Some(());
            }
        }

        self.bus_is_write = true;
        self.bus_is_data = true;
        self.bus_is_mem = false;
        self.bus_addr = addr as u32;
        self.bus_transfer_state = BusTransferState::T1;
        self.bus_data = num.to_unsigned().to_u32().unwrap();
        self.bus_data_size = count as usize;
        None
    }
}

impl Clocked<BusState, BusState> for Intel80386 {
    fn rising_edge(&mut self, state: BusState) -> BusState {
        println!("cpu rising on {:?}", self.instr_state);

        if self.bus_transfer_state == BusTransferState::T2 {
            self.bus_transfer_cycle(&state);
        }

        if self.bus_transfer_state == BusTransferState::Idle {
            self.instr_cycle(&state);
        }
        
        if self.bus_transfer_state == BusTransferState::T1 {
            self.bus_transfer_cycle(&state);
        }

        BusState::new()
    }

    fn falling_edge(&mut self, state: BusState) -> BusState {
        println!("cpu falling");
        let mut new_state = BusState::new();

        match self.bus_transfer_state {
            BusTransferState::T1 => {
                debug!("write to bus");
                new_state.assert(BusLine::ADS, Signal::Low);
                new_state.assert(BusLine::W_R,  if self.bus_is_write { Signal::High } else { Signal::Low });
                new_state.assert(BusLine::D_C,  if self.bus_is_data  { Signal::High } else { Signal::Low });
                new_state.assert(BusLine::M_IO, if self.bus_is_mem   { Signal::High } else { Signal::Low });
                if self.bus_is_write {
                    new_state.assert_address_and_data(self.bus_addr, self.bus_data, self.bus_data_size);
                } else {
                    new_state.assert_address(self.bus_addr, self.bus_data_size);
                }

                self.bus_transfer_state = BusTransferState::T2;
            }
            _ => { }
        };

        new_state
    }
}
