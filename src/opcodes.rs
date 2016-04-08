
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum AddressingMethod {
    A, C, D, E, F, G, I, J, M, O, R, S, T, X, Y
}

impl AddressingMethod {
    pub fn has_modrm(&self) -> bool {
        match *self {
            AddressingMethod::C |
            AddressingMethod::D |
            AddressingMethod::E |
            AddressingMethod::G |
            AddressingMethod::M |
            AddressingMethod::R |
            AddressingMethod::S |
            AddressingMethod::T
              => true,
            _ => false
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum OperantType {
    a, b, c, d, p, s, v, w
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct UnresolvedOperand {
    pub addr_method: AddressingMethod,
    pub op_type: OperantType
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnresolvedRegister {
    eAX,
    eCX,
    eDX,
    eBX,
    eSP,
    eBP,
    eSI,
    eDI,
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
    ES,
    SS,
    CS,
    DS,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnresolvedOp {
    Operand(UnresolvedOperand),
    Register(UnresolvedRegister)
}

impl UnresolvedOp {
    pub fn has_modrm(&self) -> bool {
        match *self {
            UnresolvedOp::Operand(operand) => operand.addr_method.has_modrm(),
            _ => false
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnresolvedOperands {
    NotImplemented,
    Invalid,
    None,
    Single(UnresolvedOp),
    Double(UnresolvedOp, UnresolvedOp),
    Triple(UnresolvedOp, UnresolvedOp, UnresolvedOp),
    Group(usize)
}

impl UnresolvedOperands {
    pub fn has_modrm(&self) -> bool {
        match *self {
            UnresolvedOperands::Single(op1) => op1.has_modrm(),
            UnresolvedOperands::Double(op1, op2) => op1.has_modrm() || op2.has_modrm(),
            UnresolvedOperands::Triple(op1, op2, op3) => op1.has_modrm() || op2.has_modrm() || op3.has_modrm(),
            UnresolvedOperands::Group(grp) => true,
            _ => false,
        }
    }
}

// Register: fixed or variable size
// Op: Operand OR Register
// opcode has one of:
// * None
// * Op
// * Op Op
// * Op Op Op
// * Group

macro_rules! op {
    (reg $reg1:ident) =>
        (UnresolvedOperands::Single(
                UnresolvedOp::Register(UnresolvedRegister::$reg1)
                ));
    ($addr1:ident $opty1:ident) =>
        (UnresolvedOperands::Single(
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr1, op_type: OperantType::$opty1 })
                ));

    (reg $reg1:ident, reg $reg2:ident) =>
        (UnresolvedOperands::Double(
                UnresolvedOp::Register(UnresolvedRegister::$reg1),
                UnresolvedOp::Register(UnresolvedRegister::$reg2)
                ));
    (reg $reg1:ident, $addr2:ident $opty2:ident) =>
        (UnresolvedOperands::Double(
                UnresolvedOp::Register(UnresolvedRegister::$reg1),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr2, op_type: OperantType::$opty2 })
                ));
    ($addr1:ident $opty1:ident, reg $reg2:ident) =>
        (UnresolvedOperands::Double(
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr1, op_type: OperantType::$opty1 }),
                UnresolvedOp::Register(UnresolvedRegister::$reg2)
                ));
    ($addr1:ident $opty1:ident, $addr2:ident $opty2:ident) =>
        (UnresolvedOperands::Double(
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr1, op_type: OperantType::$opty1 }),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr2, op_type: OperantType::$opty2 })
                ));

    (reg $reg1:ident, reg $reg2:ident, reg $reg3:ident) =>
        (UnresolvedOperands::Triple(
                UnresolvedOp::Register(UnresolvedRegister::$reg1),
                UnresolvedOp::Register(UnresolvedRegister::$reg2),
                UnresolvedOp::Register(UnresolvedRegister::$reg3)
                ));
    (reg $reg1:ident, reg $reg2:ident, $addr3:ident $opty3:ident) =>
        (UnresolvedOperands::Triple(
                UnresolvedOp::Register(UnresolvedRegister::$reg1),
                UnresolvedOp::Register(UnresolvedRegister::$reg2),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr3, op_type: OperantType::$opty3 })
                ));
    (reg $reg1:ident, $addr2:ident $opty2:ident, reg $reg3:ident) =>
        (UnresolvedOperands::Triple(
                UnresolvedOp::Register(UnresolvedRegister::$reg1),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr2, op_type: OperantType::$opty2 }),
                UnresolvedOp::Register(UnresolvedRegister::$reg3)
                ));
    (reg $reg1:ident, $addr2:ident $opty2:ident, $addr3:ident $opty3:ident) =>
        (UnresolvedOperands::Triple(
                UnresolvedOp::Register(UnresolvedRegister::$reg1),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr2, op_type: OperantType::$opty2 }),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr3, op_type: OperantType::$opty3 })
                ));
    ($addr1:ident $opty1:ident, reg $reg2:ident, reg $reg3:ident) =>
        (UnresolvedOperands::Triple(
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr1, op_type: OperantType::$opty1 }),
                UnresolvedOp::Register(UnresolvedRegister::$reg2),
                UnresolvedOp::Register(UnresolvedRegister::$reg3)
                ));
    ($addr1:ident $opty1:ident, reg $reg2:ident, $addr3:ident $opty3:ident) =>
        (UnresolvedOperands::Triple(
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr1, op_type: OperantType::$opty1 }),
                UnresolvedOp::Register(UnresolvedRegister::$reg2),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr3, op_type: OperantType::$opty3 })
                ));
    ($addr1:ident $opty1:ident, $addr2:ident $opty2:ident, reg $reg3:ident) =>
        (UnresolvedOperands::Triple(
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr1, op_type: OperantType::$opty1 }),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr2, op_type: OperantType::$opty2 }),
                UnresolvedOp::Register(UnresolvedRegister::$reg3)
                ));
    ($addr1:ident $opty1:ident, $addr2:ident $opty2:ident, $addr3:ident $opty3:ident) =>
        (UnresolvedOperands::Triple(
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr1, op_type: OperantType::$opty1 }),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr2, op_type: OperantType::$opty2 }),
                UnresolvedOp::Operand(UnresolvedOperand { addr_method: AddressingMethod::$addr3, op_type: OperantType::$opty3 })
                ));
}

macro_rules! grp {
    ($num:expr) => (UnresolvedOperands::Group($num));
}

macro_rules! no_impl {
    () => (UnresolvedOperands::NotImplemented)
}

macro_rules! inval {
    () => (UnresolvedOperands::Invalid)
}

macro_rules! none {
    () => (UnresolvedOperands::None)
}

pub static SINGLE_OPCODE_MAP: [UnresolvedOperands; 256] = [
        /* 0x00 */ op!(E b, G b),
        /* 0x01 */ op!(E v, G v),
        /* 0x02 */ op!(G b, E b),
        /* 0x03 */ op!(G v, E v),
        /* 0x04 */ op!(reg AL, I b),
        /* 0x05 */ op!(reg eAX, I v),
        /* 0x06 */ op!(reg ES),
        /* 0x07 */ op!(reg ES),
        /* 0x08 */ op!(E b, G b),
        /* 0x09 */ no_impl!(),
        /* 0x0A */ no_impl!(),
        /* 0x0B */ no_impl!(),
        /* 0x0C */ no_impl!(),
        /* 0x0D */ no_impl!(),
        /* 0x0E */ no_impl!(),
        /* 0x0F */ inval!(),

        /* 0x10 */ no_impl!(),
        /* 0x11 */ no_impl!(),
        /* 0x12 */ no_impl!(),
        /* 0x13 */ no_impl!(),
        /* 0x14 */ no_impl!(),
        /* 0x15 */ no_impl!(),
        /* 0x16 */ no_impl!(),
        /* 0x17 */ no_impl!(),
        /* 0x18 */ no_impl!(),
        /* 0x19 */ no_impl!(),
        /* 0x1A */ no_impl!(),
        /* 0x1B */ no_impl!(),
        /* 0x1C */ no_impl!(),
        /* 0x1D */ no_impl!(),
        /* 0x1E */ no_impl!(),
        /* 0x1F */ no_impl!(),

        /* 0x20 */ no_impl!(),
        /* 0x21 */ no_impl!(),
        /* 0x22 */ no_impl!(),
        /* 0x23 */ no_impl!(),
        /* 0x24 */ no_impl!(),
        /* 0x25 */ no_impl!(),
        /* 0x26 */ inval!(),
        /* 0x27 */ no_impl!(),
        /* 0x28 */ no_impl!(),
        /* 0x29 */ no_impl!(),
        /* 0x2A */ no_impl!(),
        /* 0x2B */ no_impl!(),
        /* 0x2C */ no_impl!(),
        /* 0x2D */ no_impl!(),
        /* 0x2E */ inval!(),
        /* 0x2F */ no_impl!(),

        /* 0x30 */ no_impl!(),
        /* 0x31 */ no_impl!(),
        /* 0x32 */ no_impl!(),
        /* 0x33 */ no_impl!(),
        /* 0x34 */ no_impl!(),
        /* 0x35 */ no_impl!(),
        /* 0x36 */ inval!(),
        /* 0x37 */ no_impl!(),
        /* 0x38 */ no_impl!(),
        /* 0x39 */ no_impl!(),
        /* 0x3A */ no_impl!(),
        /* 0x3B */ no_impl!(),
        /* 0x3C */ no_impl!(),
        /* 0x3D */ no_impl!(),
        /* 0x3E */ inval!(),
        /* 0x3F */ no_impl!(),

        /* 0x40 */ no_impl!(),
        /* 0x41 */ no_impl!(),
        /* 0x42 */ no_impl!(),
        /* 0x43 */ no_impl!(),
        /* 0x44 */ no_impl!(),
        /* 0x45 */ no_impl!(),
        /* 0x46 */ no_impl!(),
        /* 0x47 */ no_impl!(),
        /* 0x48 */ no_impl!(),
        /* 0x49 */ no_impl!(),
        /* 0x4A */ no_impl!(),
        /* 0x4B */ no_impl!(),
        /* 0x4C */ no_impl!(),
        /* 0x4D */ no_impl!(),
        /* 0x4E */ no_impl!(),
        /* 0x4F */ no_impl!(),

        /* 0x50 */ no_impl!(),
        /* 0x51 */ no_impl!(),
        /* 0x52 */ no_impl!(),
        /* 0x53 */ no_impl!(),
        /* 0x54 */ no_impl!(),
        /* 0x55 */ no_impl!(),
        /* 0x56 */ no_impl!(),
        /* 0x57 */ no_impl!(),
        /* 0x58 */ no_impl!(),
        /* 0x59 */ no_impl!(),
        /* 0x5A */ no_impl!(),
        /* 0x5B */ no_impl!(),
        /* 0x5C */ no_impl!(),
        /* 0x5D */ no_impl!(),
        /* 0x5E */ no_impl!(),
        /* 0x5F */ no_impl!(),

        /* 0x60 */ no_impl!(),
        /* 0x61 */ no_impl!(),
        /* 0x62 */ no_impl!(),
        /* 0x63 */ no_impl!(),
        /* 0x64 */ inval!(),
        /* 0x65 */ inval!(),
        /* 0x66 */ inval!(),
        /* 0x67 */ inval!(),
        /* 0x68 */ no_impl!(),
        /* 0x69 */ no_impl!(),
        /* 0x6A */ no_impl!(),
        /* 0x6B */ op!(G v, E v, I v),
        /* 0x6C */ no_impl!(),
        /* 0x6D */ no_impl!(),
        /* 0x6E */ no_impl!(),
        /* 0x6F */ no_impl!(),

        /* 0x70 */ no_impl!(),
        /* 0x71 */ no_impl!(),
        /* 0x72 */ no_impl!(),
        /* 0x73 */ no_impl!(),
        /* 0x74 */ no_impl!(),
        /* 0x75 */ no_impl!(),
        /* 0x76 */ no_impl!(),
        /* 0x77 */ no_impl!(),
        /* 0x78 */ no_impl!(),
        /* 0x79 */ no_impl!(),
        /* 0x7A */ no_impl!(),
        /* 0x7B */ no_impl!(),
        /* 0x7C */ no_impl!(),
        /* 0x7D */ no_impl!(),
        /* 0x7E */ no_impl!(),
        /* 0x7F */ no_impl!(),

        /* 0x80 */ no_impl!(),
        /* 0x81 */ no_impl!(),
        /* 0x82 */ inval!(),
        /* 0x83 */ no_impl!(),
        /* 0x84 */ no_impl!(),
        /* 0x85 */ no_impl!(),
        /* 0x86 */ no_impl!(),
        /* 0x87 */ no_impl!(),
        /* 0x88 */ no_impl!(),
        /* 0x89 */ no_impl!(),
        /* 0x8A */ no_impl!(),
        /* 0x8B */ no_impl!(),
        /* 0x8C */ no_impl!(),
        /* 0x8D */ no_impl!(),
        /* 0x8E */ no_impl!(),
        /* 0x8F */ no_impl!(),

        /* 0x90 */ no_impl!(),
        /* 0x91 */ no_impl!(),
        /* 0x92 */ no_impl!(),
        /* 0x93 */ no_impl!(),
        /* 0x94 */ no_impl!(),
        /* 0x95 */ no_impl!(),
        /* 0x96 */ no_impl!(),
        /* 0x97 */ no_impl!(),
        /* 0x98 */ no_impl!(),
        /* 0x99 */ no_impl!(),
        /* 0x9A */ no_impl!(),
        /* 0x9B */ no_impl!(),
        /* 0x9C */ no_impl!(),
        /* 0x9D */ no_impl!(),
        /* 0x9E */ no_impl!(),
        /* 0x9F */ no_impl!(),

        /* 0xA0 */ no_impl!(),
        /* 0xA1 */ no_impl!(),
        /* 0xA2 */ no_impl!(),
        /* 0xA3 */ no_impl!(),
        /* 0xA4 */ no_impl!(),
        /* 0xA5 */ no_impl!(),
        /* 0xA6 */ no_impl!(),
        /* 0xA7 */ no_impl!(),
        /* 0xA8 */ no_impl!(),
        /* 0xA9 */ no_impl!(),
        /* 0xAA */ no_impl!(),
        /* 0xAB */ no_impl!(),
        /* 0xAC */ no_impl!(),
        /* 0xAD */ no_impl!(),
        /* 0xAE */ no_impl!(),
        /* 0xAF */ no_impl!(),

        /* 0xB0 */ no_impl!(),
        /* 0xB1 */ no_impl!(),
        /* 0xB2 */ no_impl!(),
        /* 0xB3 */ no_impl!(),
        /* 0xB4 */ no_impl!(),
        /* 0xB5 */ no_impl!(),
        /* 0xB6 */ no_impl!(),
        /* 0xB7 */ no_impl!(),
        /* 0xB8 */ no_impl!(),
        /* 0xB9 */ no_impl!(),
        /* 0xBA */ no_impl!(),
        /* 0xBB */ no_impl!(),
        /* 0xBC */ no_impl!(),
        /* 0xBD */ no_impl!(),
        /* 0xBE */ no_impl!(),
        /* 0xBF */ no_impl!(),

        /* 0xC0 */ no_impl!(),
        /* 0xC1 */ no_impl!(),
        /* 0xC2 */ no_impl!(),
        /* 0xC3 */ no_impl!(),
        /* 0xC4 */ no_impl!(),
        /* 0xC5 */ no_impl!(),
        /* 0xC6 */ no_impl!(),
        /* 0xC7 */ no_impl!(),
        /* 0xC8 */ no_impl!(),
        /* 0xC9 */ no_impl!(),
        /* 0xCA */ no_impl!(),
        /* 0xCB */ no_impl!(),
        /* 0xCC */ no_impl!(),
        /* 0xCD */ no_impl!(),
        /* 0xCE */ no_impl!(),
        /* 0xCF */ no_impl!(),

        /* 0xD0 */ no_impl!(),
        /* 0xD1 */ no_impl!(),
        /* 0xD2 */ no_impl!(),
        /* 0xD3 */ no_impl!(),
        /* 0xD4 */ no_impl!(),
        /* 0xD5 */ no_impl!(),
        /* 0xD6 */ inval!(),
        /* 0xD7 */ no_impl!(),
        /* 0xD8 */ no_impl!(),
        /* 0xD9 */ no_impl!(),
        /* 0xDA */ no_impl!(),
        /* 0xDB */ no_impl!(),
        /* 0xDC */ no_impl!(),
        /* 0xDD */ no_impl!(),
        /* 0xDE */ no_impl!(),
        /* 0xDF */ no_impl!(),

        /* 0xE0 */ no_impl!(),
        /* 0xE1 */ no_impl!(),
        /* 0xE2 */ no_impl!(),
        /* 0xE3 */ no_impl!(),
        /* 0xE4 */ no_impl!(),
        /* 0xE5 */ no_impl!(),
        /* 0xE6 */ no_impl!(),
        /* 0xE7 */ no_impl!(),
        /* 0xE8 */ no_impl!(),
        /* 0xE9 */ no_impl!(),
        /* 0xEA */ op!(A p),
        /* 0xEB */ no_impl!(),
        /* 0xEC */ no_impl!(),
        /* 0xED */ no_impl!(),
        /* 0xEE */ no_impl!(),
        /* 0xEF */ no_impl!(),

        /* 0xF0 */ inval!(),
        /* 0xF1 */ inval!(),
        /* 0xF2 */ no_impl!(),
        /* 0xF3 */ no_impl!(),
        /* 0xF4 */ no_impl!(),
        /* 0xF5 */ no_impl!(),
        /* 0xF6 */ no_impl!(),
        /* 0xF7 */ no_impl!(),
        /* 0xF8 */ no_impl!(),
        /* 0xF9 */ no_impl!(),
        /* 0xFA */ no_impl!(),
        /* 0xFB */ no_impl!(),
        /* 0xFC */ no_impl!(),
        /* 0xFD */ no_impl!(),
        /* 0xFE */ no_impl!(),
        /* 0xFF */ grp!(5),
];

pub static DOUBLE_OPCODE_MAP: [UnresolvedOperands; 256] = [
        /* 0x00 */ no_impl!(),
        /* 0x01 */ no_impl!(),
        /* 0x02 */ no_impl!(),
        /* 0x03 */ no_impl!(),
        /* 0x04 */ inval!(),
        /* 0x05 */ inval!(),
        /* 0x06 */ no_impl!(),
        /* 0x07 */ inval!(),
        /* 0x08 */ inval!(),
        /* 0x09 */ inval!(),
        /* 0x0A */ inval!(),
        /* 0x0B */ inval!(),
        /* 0x0C */ inval!(),
        /* 0x0D */ inval!(),
        /* 0x0E */ inval!(),
        /* 0x0F */ inval!(),

        /* 0x10 */ inval!(),
        /* 0x11 */ inval!(),
        /* 0x12 */ inval!(),
        /* 0x13 */ inval!(),
        /* 0x14 */ inval!(),
        /* 0x15 */ inval!(),
        /* 0x16 */ inval!(),
        /* 0x17 */ inval!(),
        /* 0x18 */ inval!(),
        /* 0x19 */ inval!(),
        /* 0x1A */ inval!(),
        /* 0x1B */ inval!(),
        /* 0x1C */ inval!(),
        /* 0x1D */ inval!(),
        /* 0x1E */ inval!(),
        /* 0x1F */ inval!(),

        /* 0x20 */ no_impl!(),
        /* 0x21 */ no_impl!(),
        /* 0x22 */ no_impl!(),
        /* 0x23 */ no_impl!(),
        /* 0x24 */ no_impl!(),
        /* 0x25 */ inval!(),
        /* 0x26 */ no_impl!(),
        /* 0x27 */ inval!(),
        /* 0x28 */ inval!(),
        /* 0x29 */ inval!(),
        /* 0x2A */ inval!(),
        /* 0x2B */ inval!(),
        /* 0x2C */ inval!(),
        /* 0x2D */ inval!(),
        /* 0x2E */ inval!(),
        /* 0x2F */ inval!(),

        /* 0x30 */ inval!(),
        /* 0x31 */ inval!(),
        /* 0x32 */ inval!(),
        /* 0x33 */ inval!(),
        /* 0x34 */ inval!(),
        /* 0x35 */ inval!(),
        /* 0x36 */ inval!(),
        /* 0x37 */ inval!(),
        /* 0x38 */ inval!(),
        /* 0x39 */ inval!(),
        /* 0x3A */ inval!(),
        /* 0x3B */ inval!(),
        /* 0x3C */ inval!(),
        /* 0x3D */ inval!(),
        /* 0x3E */ inval!(),
        /* 0x3F */ inval!(),

        /* 0x40 */ inval!(),
        /* 0x41 */ inval!(),
        /* 0x42 */ inval!(),
        /* 0x43 */ inval!(),
        /* 0x44 */ inval!(),
        /* 0x45 */ inval!(),
        /* 0x46 */ inval!(),
        /* 0x47 */ inval!(),
        /* 0x48 */ inval!(),
        /* 0x49 */ inval!(),
        /* 0x4A */ inval!(),
        /* 0x4B */ inval!(),
        /* 0x4C */ inval!(),
        /* 0x4D */ inval!(),
        /* 0x4E */ inval!(),
        /* 0x4F */ inval!(),

        /* 0x50 */ inval!(),
        /* 0x51 */ inval!(),
        /* 0x52 */ inval!(),
        /* 0x53 */ inval!(),
        /* 0x54 */ inval!(),
        /* 0x55 */ inval!(),
        /* 0x56 */ inval!(),
        /* 0x57 */ inval!(),
        /* 0x58 */ inval!(),
        /* 0x59 */ inval!(),
        /* 0x5A */ inval!(),
        /* 0x5B */ inval!(),
        /* 0x5C */ inval!(),
        /* 0x5D */ inval!(),
        /* 0x5E */ inval!(),
        /* 0x5F */ inval!(),

        /* 0x60 */ inval!(),
        /* 0x61 */ inval!(),
        /* 0x62 */ inval!(),
        /* 0x63 */ inval!(),
        /* 0x64 */ inval!(),
        /* 0x65 */ inval!(),
        /* 0x66 */ inval!(),
        /* 0x67 */ inval!(),
        /* 0x68 */ inval!(),
        /* 0x69 */ inval!(),
        /* 0x6A */ inval!(),
        /* 0x6B */ inval!(),
        /* 0x6C */ inval!(),
        /* 0x6D */ inval!(),
        /* 0x6E */ inval!(),
        /* 0x6F */ inval!(),

        /* 0x70 */ inval!(),
        /* 0x71 */ inval!(),
        /* 0x72 */ inval!(),
        /* 0x73 */ inval!(),
        /* 0x74 */ inval!(),
        /* 0x75 */ inval!(),
        /* 0x76 */ inval!(),
        /* 0x77 */ inval!(),
        /* 0x78 */ inval!(),
        /* 0x79 */ inval!(),
        /* 0x7A */ inval!(),
        /* 0x7B */ inval!(),
        /* 0x7C */ inval!(),
        /* 0x7D */ inval!(),
        /* 0x7E */ inval!(),
        /* 0x7F */ inval!(),

        /* 0x80 */ no_impl!(),
        /* 0x81 */ no_impl!(),
        /* 0x82 */ no_impl!(),
        /* 0x83 */ no_impl!(),
        /* 0x84 */ no_impl!(),
        /* 0x85 */ no_impl!(),
        /* 0x86 */ no_impl!(),
        /* 0x87 */ no_impl!(),
        /* 0x88 */ no_impl!(),
        /* 0x89 */ no_impl!(),
        /* 0x8A */ no_impl!(),
        /* 0x8B */ no_impl!(),
        /* 0x8C */ no_impl!(),
        /* 0x8D */ no_impl!(),
        /* 0x8E */ no_impl!(),
        /* 0x8F */ no_impl!(),

        /* 0x90 */ no_impl!(),
        /* 0x91 */ no_impl!(),
        /* 0x92 */ no_impl!(),
        /* 0x93 */ no_impl!(),
        /* 0x94 */ no_impl!(),
        /* 0x95 */ no_impl!(),
        /* 0x96 */ no_impl!(),
        /* 0x97 */ no_impl!(),
        /* 0x98 */ no_impl!(),
        /* 0x99 */ no_impl!(),
        /* 0x9A */ no_impl!(),
        /* 0x9B */ no_impl!(),
        /* 0x9C */ no_impl!(),
        /* 0x9D */ no_impl!(),
        /* 0x9E */ no_impl!(),
        /* 0x9F */ no_impl!(),

        /* 0xA0 */ no_impl!(),
        /* 0xA1 */ no_impl!(),
        /* 0xA2 */ inval!(),
        /* 0xA3 */ no_impl!(),
        /* 0xA4 */ no_impl!(),
        /* 0xA5 */ no_impl!(),
        /* 0xA6 */ inval!(),
        /* 0xA7 */ inval!(),
        /* 0xA8 */ no_impl!(),
        /* 0xA9 */ no_impl!(),
        /* 0xAA */ inval!(),
        /* 0xAB */ no_impl!(),
        /* 0xAC */ no_impl!(),
        /* 0xAD */ no_impl!(),
        /* 0xAE */ inval!(),
        /* 0xAF */ no_impl!(),

        /* 0xB0 */ inval!(),
        /* 0xB1 */ inval!(),
        /* 0xB2 */ no_impl!(),
        /* 0xB3 */ no_impl!(),
        /* 0xB4 */ no_impl!(),
        /* 0xB5 */ no_impl!(),
        /* 0xB6 */ no_impl!(),
        /* 0xB7 */ no_impl!(),
        /* 0xB8 */ inval!(),
        /* 0xB9 */ inval!(),
        /* 0xBA */ no_impl!(),
        /* 0xBB */ no_impl!(),
        /* 0xBC */ no_impl!(),
        /* 0xBD */ no_impl!(),
        /* 0xBE */ no_impl!(),
        /* 0xBF */ no_impl!(),

        /* 0xC0 */ inval!(),
        /* 0xC1 */ inval!(),
        /* 0xC2 */ inval!(),
        /* 0xC3 */ inval!(),
        /* 0xC4 */ inval!(),
        /* 0xC5 */ inval!(),
        /* 0xC6 */ inval!(),
        /* 0xC7 */ inval!(),
        /* 0xC8 */ inval!(),
        /* 0xC9 */ inval!(),
        /* 0xCA */ inval!(),
        /* 0xCB */ inval!(),
        /* 0xCC */ inval!(),
        /* 0xCD */ inval!(),
        /* 0xCE */ inval!(),
        /* 0xCF */ inval!(),

        /* 0xD0 */ inval!(),
        /* 0xD1 */ inval!(),
        /* 0xD2 */ inval!(),
        /* 0xD3 */ inval!(),
        /* 0xD4 */ inval!(),
        /* 0xD5 */ inval!(),
        /* 0xD6 */ inval!(),
        /* 0xD7 */ inval!(),
        /* 0xD8 */ inval!(),
        /* 0xD9 */ inval!(),
        /* 0xDA */ inval!(),
        /* 0xDB */ inval!(),
        /* 0xDC */ inval!(),
        /* 0xDD */ inval!(),
        /* 0xDE */ inval!(),
        /* 0xDF */ inval!(),

        /* 0xE0 */ inval!(),
        /* 0xE1 */ inval!(),
        /* 0xE2 */ inval!(),
        /* 0xE3 */ inval!(),
        /* 0xE4 */ inval!(),
        /* 0xE5 */ inval!(),
        /* 0xE6 */ inval!(),
        /* 0xE7 */ inval!(),
        /* 0xE8 */ inval!(),
        /* 0xE9 */ inval!(),
        /* 0xEA */ inval!(),
        /* 0xEB */ inval!(),
        /* 0xEC */ inval!(),
        /* 0xED */ inval!(),
        /* 0xEE */ inval!(),
        /* 0xEF */ inval!(),

        /* 0xF0 */ inval!(),
        /* 0xF1 */ inval!(),
        /* 0xF2 */ inval!(),
        /* 0xF3 */ inval!(),
        /* 0xF4 */ inval!(),
        /* 0xF5 */ inval!(),
        /* 0xF6 */ inval!(),
        /* 0xF7 */ inval!(),
        /* 0xF8 */ inval!(),
        /* 0xF9 */ inval!(),
        /* 0xFA */ inval!(),
        /* 0xFB */ inval!(),
        /* 0xFC */ inval!(),
        /* 0xFD */ inval!(),
        /* 0xFE */ inval!(),
        /* 0xFF */ inval!(),
];

pub static GROUP_MAP: [[UnresolvedOperands; 8]; 8] = [
    // Group 1
    [
        /* 0b000 */ none!(),
        /* 0b001 */ none!(),
        /* 0b010 */ none!(),
        /* 0b011 */ none!(),
        /* 0b100 */ none!(),
        /* 0b101 */ none!(),
        /* 0b110 */ none!(),
        /* 0b111 */ none!(),
    ],
    // Group 2
    [
        /* 0b000 */ none!(),
        /* 0b001 */ none!(),
        /* 0b010 */ none!(),
        /* 0b011 */ none!(),
        /* 0b100 */ none!(),
        /* 0b101 */ none!(),
        /* 0b110 */ inval!(),
        /* 0b111 */ none!(),
    ],
     // Group 3
    [
        /* 0b000 */ no_impl!(),
        /* 0b001 */ inval!(),
        /* 0b010 */ none!(),
        /* 0b011 */ none!(),
        /* 0b100 */ no_impl!(),
        /* 0b101 */ no_impl!(),
        /* 0b110 */ no_impl!(),
        /* 0b111 */ no_impl!(),
    ],
     // Group 4
    [
        /* 0b000 */ no_impl!(),
        /* 0b001 */ no_impl!(),
        /* 0b010 */ inval!(),
        /* 0b011 */ inval!(),
        /* 0b100 */ inval!(),
        /* 0b101 */ inval!(),
        /* 0b110 */ inval!(),
        /* 0b111 */ inval!(),
    ],
    // Group 5
    [
        /* 0b000 */ op!(E v),
        /* 0b001 */ op!(E v),
        /* 0b010 */ op!(E v),
        /* 0b011 */ no_impl!(),
        /* 0b100 */ op!(E v),
        /* 0b101 */ op!(E p),
        /* 0b110 */ op!(E v),
        /* 0b111 */ inval!(),
    ],
    // Group 6
    [
        /* 0b000 */ no_impl!(),
        /* 0b001 */ no_impl!(),
        /* 0b010 */ no_impl!(),
        /* 0b011 */ no_impl!(),
        /* 0b100 */ no_impl!(),
        /* 0b101 */ no_impl!(),
        /* 0b110 */ inval!(),
        /* 0b111 */ inval!(),
    ],
     // Group 7
    [
        /* 0b000 */ no_impl!(),
        /* 0b001 */ no_impl!(),
        /* 0b010 */ no_impl!(),
        /* 0b011 */ no_impl!(),
        /* 0b100 */ no_impl!(),
        /* 0b101 */ inval!(),
        /* 0b110 */ no_impl!(),
        /* 0b111 */ inval!(),
    ],
     // Group 8
    [
        /* 0b000 */ inval!(),
        /* 0b001 */ inval!(),
        /* 0b010 */ inval!(),
        /* 0b011 */ inval!(),
        /* 0b100 */ none!(),
        /* 0b101 */ none!(),
        /* 0b110 */ none!(),
        /* 0b111 */ none!(),
    ],
];
