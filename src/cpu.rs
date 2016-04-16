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
use reg::{ SegmentRegister, Register, RegisterFile, EFlags, RegEnum };
use instr::{
    InstructionPrefix, SegmentOverridePrefix, Instruction,
    Operands, Op
};
use num::{ Num, Size, Type };

type VAddr  = u32;
type SAddr  = (u16, u16);
type PAddr  = u32;
type IOAddr = u16;

#[derive(PartialEq, Eq, Debug)]
enum BusTransferState {
    Idle,
    T1,
    T2
}

#[derive(PartialEq, Eq, Debug)]
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
    MemRead,

    GEN_OP_Read,
    GEN_OP_Write,

    ADD_0,
    ADC_1,
    AND_2,
    XOR_3,
    OR_0,
    SBB_1,
    SUB_2,
    CMP_3,

    MOV_Bx,

    IN_0,

    JMP_EA,

    HALT,

    MemWrite,
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

    eip: u32,
    has_ljmped: bool,
    gen_regs: RegisterFile<Register>,
    seg_regs: RegisterFile<SegmentRegister>,
    eflags: EFlags,

    instr_eip: u32,
    instr_byte: u8,
    instr_buf: u32,
    instr_buf_addr: u32,
    instr_did_consume_byte: bool,

    current_instr: Instruction,
    op1val: Option<Num>,
    op2val: Option<Num>,
    op3val: Option<Num>,

    bus_is_write: bool, // vs write
    bus_is_data: bool, // vs control
    bus_is_mem: bool, // vs io
    bus_addr: PAddr,
    bus_data: u32,
    bus_data_size: usize,

    io_read_result: Option<(IOAddr, u32)>
}

impl Intel80386 {
    pub fn new() -> Intel80386 {
        let mut i386 = Intel80386 {
            bus_transfer_state: BusTransferState::Idle,
            instr_state: InstructionState::Fetch0,
            cache: Cache::new(),
            roms: vec![],

            eip: 0,
            has_ljmped: false,
            gen_regs: RegisterFile::new(8),
            seg_regs: RegisterFile::new(6),
            eflags: ::reg::EFLAGS_RESERVED1,

            instr_eip: 0,
            instr_byte: 0,
            instr_buf: 0,
            instr_buf_addr: 0,
            instr_did_consume_byte: true,

            current_instr: Instruction::new(),
            op1val: None,
            op2val: None,
            op3val: None,

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

    fn instr_cycle(&mut self, state: &BusState) {
        // Instruction Prefix
        if self.instr_state == InstructionState::Fetch0 {
            debug!("START EIP: {:08x}", self.eip());
            self.instr_eip = self.eip().to_u32().unwrap();
            self.instr_did_consume_byte = true;
            self.current_instr = Instruction::new();
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
            match self.instr_byte {
                0x67 => {
                    self.current_instr.addr_sz_prefix = true;
                    self.current_instr.addr_sz = !self.default_size();
                },
                _ => {
                    self.current_instr.addr_sz_prefix = false;
                    self.current_instr.addr_sz = self.default_size();
                }
            }

            debug!("instr address size: {:?}", self.current_instr.addr_sz_prefix);
        },
        consumed if self.current_instr.addr_sz_prefix == true];


        // Operand Size Prefix
        instr_state![self, Fetch2 -> Fetch3, {
            match self.instr_byte {
                0x66 => {
                    self.current_instr.op_sz_prefix = true;
                    self.current_instr.op_sz = !self.default_size();
                },
                _ => {
                    self.current_instr.op_sz_prefix = false;
                    self.current_instr.op_sz = self.default_size();
                }
            };

            debug!("operand size prefix: {:?}", self.current_instr.op_sz_prefix);
        },
        consumed if self.current_instr.op_sz_prefix == true];


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
                debug!("modrm: {:02x}", self.instr_byte);
                self.current_instr.modrm = Some(self.instr_byte);
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
                self.current_instr.disp = (self.instr_byte as u32) << 8 * (self.current_instr.disp_sz - 1);
            }
        },
        consumed if self.current_instr.disp_sz > 0,
        -> Fetch12 if self.current_instr.disp_sz <= 1];


        // Displacement Byte 2
        instr_state![self, Fetch9 -> Fetch10, {
            self.current_instr.disp |= (self.instr_byte as u32) << 8 * (self.current_instr.disp_sz - 2);
        },
        consumed if true,
        -> Fetch12 if self.current_instr.disp_sz == 2];


        // Displacement Byte 3
        instr_state![self, Fetch10 -> Fetch11, {
            self.current_instr.disp |= (self.instr_byte as u32) << 8 * (self.current_instr.disp_sz - 3);
        },
        consumed if true,
        -> Fetch12 if self.current_instr.disp_sz == 3];


        // Displacement Byte 4
        instr_state![self, Fetch11 -> Fetch12, {
            self.current_instr.disp |= (self.instr_byte as u32) << 8 * (self.current_instr.disp_sz - 4);
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

        instr_state![self, Decode -> MemRead, {
            debug!("{:?}", self.current_instr);
            self.eip = self.instr_eip - 1;
            // unimplemented!();
        }];

        instr_state![self, MemRead -> MemRead, {
            
        },
        -> GEN_OP_Read if self.current_instr.opcode & 0xC2 == 0x00,
        -> MOV_Bx if self.current_instr.opcode & 0xF0 == 0xB0,
        -> JMP_EA if self.current_instr.opcode == 0xEA,
        -> IN_0 if self.current_instr.opcode & 0xF6 == 0xE4
        ];

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

        }];

        instr_state![self, ADC_1 -> GEN_OP_Write, {
            
        }];

        instr_state![self, AND_2 -> GEN_OP_Write, {
            
        }];

        instr_state![self, XOR_3 -> GEN_OP_Write, {
            
        }];

        instr_state![self, OR_0 -> GEN_OP_Write, {
            
        }];

        instr_state![self, SBB_1 -> GEN_OP_Write, {
            
        }];

        instr_state![self, SUB_2 -> GEN_OP_Write, {
            
        }];

        instr_state![self, CMP_3 -> GEN_OP_Write, {
            
        }];

        instr_state![self, MOV_Bx -> Fetch0, {
            trace!("{:?}", self.current_instr.operands);
            match self.current_instr.operands {
                Operands::Double(Op::Register(reg), Op::Immediate(imm)) => {
                    self.gen_regs.write(reg, self.current_instr.imm);
                },
                _ => panic!("unexpected operand")
            };
        }];

        instr_state![self, IN_0 -> Fetch0, {
            let (dst_reg, src_port) = match self.current_instr.operands {
                Operands::Double(Op::Register(reg), Op::Immediate(imm)) =>
                    (reg, self.current_instr.imm as u16),
                Operands::Double(Op::Register(reg1), Op::Register(reg2)) =>
                    (reg1, self.gen_regs.read(reg2).0 as u16),
                _ => panic!("unexpected operands for IN_0")
            };

            let res = self.read_io_size(src_port, dst_reg.size());
            if !res.is_some() { return; }

            trace!("write io {:x} to reg {:?}", res.unwrap(), dst_reg);
            self.gen_regs.write(dst_reg, res.unwrap());
        }];

        instr_state![self, JMP_EA -> Fetch0, {
            if true /* real mode */ {
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
            } else {
                // protected mode
                unimplemented!();
            }
        }];

        instr_state![self, HALT -> HALT, {
            trace!("halt");
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
                    if state.read(BusLine::D_C) == Signal::High
                        && state.read(BusLine::W_R) == Signal::Low {

                        if state.read(BusLine::W_R) == Signal::Low {
                            match state.read(BusLine::M_IO) {
                                Signal::Low => {
                                    self.io_read_result = Some((
                                            state.read_address() as u16,
                                            state.read_data()
                                            ))
                                },
                                Signal::High => {
                                    self.cache.write(
                                        state.read_address(),
                                        state.read_data()
                                        )
                                },
                                _ => panic!("m/io undefined")
                            }
                        }
                    }
                }
            },
            _ => {

            }
        }

    }

    // Utility

    fn default_size(&self) -> Size {
        Size::Size16 // TODO
    }

    fn eip(&self) -> Num {
        // TODO: protected mode
        segaddr![self, CS:self.eip] | if self.has_ljmped { 0 } else { 0xFFF00000 }
    }

    fn read_op(&mut self, op: Op) -> Option<Num> {
        match op {
            Op::Register(reg) => Some(self.gen_regs.read(reg)),
            Op::SegmentRegister(reg) => Some(self.seg_regs.read(reg)),
            Op::FlagsRegister(sz) => Some(Num(self.eflags.bits(), sz.unsigned())),
            Op::MemoryAddress(seg_reg, base, index, scale, disp_sz, size) => {
                let addr = self.calc_address(seg_reg, base, index, scale, disp_sz);
                self.mmu_read_mem_sz(addr.to_u32().unwrap(), size)
                    .map_or(None, |val| Some(Num(val, size.unsigned())))
            },
            Op::Immediate(sz) => Some(Num(self.current_instr.imm, sz.signed())),
            _ => panic!("unhandled read op")
        }
    }

    fn calc_address(&mut self, seg_reg: SegmentRegister, base: Option<Register>,
        index: Option<Register>, scale: u8, disp_sz: Option<Size>) -> Num {
        let z = Num(0, self.current_instr.addr_sz.unsigned());
        
        self.seg_addr(seg_reg)
            + base.map_or(z, |reg| self.gen_regs.read(reg))
            + index.map_or(z, |reg| self.gen_regs.read(reg) << (scale as usize))
            + disp_sz.map_or(z,
                |sz| Num(self.current_instr.disp, sz.signed()))
    }

    fn seg_addr(&self, seg_reg: SegmentRegister) -> Num {
        // TODO: protected mode
        self.seg_regs.read(seg_reg) << 4
    }

    fn update_flags(&mut self, num: Num, cf: bool, af: bool, of: bool) {
        self.update_flag(::reg::EFLAGS_CF, cf);
        self.update_flag(::reg::EFLAGS_PF, num.low_byte_parity());
        self.update_flag(::reg::EFLAGS_AF, af); // decimal arith, not needed (?)
        self.update_flag(::reg::EFLAGS_ZF, num == 0);
        self.update_flag(::reg::EFLAGS_SF, num < 0);
        self.update_flag(::reg::EFLAGS_OF, of);
    }

    fn update_flag(&mut self, flag: EFlags, pred: bool) {
        if pred {
            self.eflags.remove(flag);
        } else {
            self.eflags.insert(flag);
        }
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
        if paddr % count != 0 { unimplemented!(); }

        let aligned_paddr = paddr >> 2;
        if let Some(val) = self.mmu_read_mem_u32_aligned(aligned_paddr) {
            let mask = 0xFFFFFFFFu32 >> (count * 8);
            let masked_val = (val >> ((paddr % 4) * 8)) & mask;
            if let Some(coverted) = T::from_u32(masked_val) {
                return Some(coverted);
            } else {
                unreachable!();
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

    fn read_io_size(&mut self, addr: IOAddr, size: Size) -> Option<u32> {
        match size {
            Size::Size8  => self.read_io::<u8 >(addr).map(|x| x as u32),
            Size::Size16 => self.read_io::<u16>(addr).map(|x| x as u32),
            Size::Size32 => self.read_io::<u32>(addr),
            _ => panic!("unsupported size")
        }
    }

    fn read_io<T>(&mut self, addr: IOAddr) -> Option<T> where T: FromPrimitive {
        assert!(self.bus_transfer_state == BusTransferState::Idle);

        let count = mem::size_of::<T>() as u32;
        assert!(count == 1 || count == 2 || count == 4, "count must be in 1, 2, 4");
        assert!((addr as u32) % count == 0, "io access must be aligned");

        if let Some((ioaddr, val)) = self.io_read_result {
            trace!("IO RESULT {:08x} {:08x} VAL {:08x}", addr, ioaddr, val);
            if addr == ioaddr {
                return Some(T::from_u32(val).unwrap());
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
