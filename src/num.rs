use std::ops::{
    Not, Add, Sub, Shl, Shr,
    BitAnd, BitOr, BitXor
};
use std::cmp::{ Ord, PartialOrd, Ordering };
use std::fmt::{ LowerHex, Formatter, Result };
use numlib::{ ToPrimitive };

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Num(pub u32, pub Type);

macro_rules! expr {
    ($e:expr) => {
        $e
    }
}

macro_rules! cast {
    ($num:ident, $ty:ty) => (match $num.1 {
        Type::u8  => ($num.0 as u8 ) as $ty,
        Type::u16 => ($num.0 as u16) as $ty,
        Type::u32 => ($num.0 as u32) as $ty,
        Type::i8  => ($num.0 as i8 ) as $ty,
        Type::i16 => ($num.0 as i16) as $ty,
        Type::i32 => ($num.0 as i32) as $ty
    })
}

macro_rules! cast_to {
    ($num:ident, $ty:ident) => (match $ty {
        Type::u8  => cast![$num, u8 ] as u32,
        Type::u16 => cast![$num, u16] as u32,
        Type::u32 => cast![$num, u32] as u32,
        Type::i8  => cast![$num, i8 ] as u32,
        Type::i16 => cast![$num, i16] as u32,
        Type::i32 => cast![$num, i32] as u32
    })
}

macro_rules! cast_op {
    ($lhs:expr; $op:ident; $rhs:expr; $ty:ident; ($ty1:ty, $ty2:ty)) => ({
        let (a, b) = ($lhs as $ty).$op($rhs as $ty);
        (a as $ty1, b as $ty2)
    });

    ($lhs:expr; $op:ident; $rhs:expr; $ty:ident; $tty:ty) =>
        (expr!((($lhs as $ty).$op($rhs as $ty)) as $tty));
}

macro_rules! sized_op {
    ($lhs:ident $op:ident $rhs:expr; $ty:tt) =>
        (match $lhs.1 {
            Type::u8  => cast_op![$lhs.0; $op; $rhs; u8 ; $ty],
            Type::u16 => cast_op![$lhs.0; $op; $rhs; u16; $ty],
            Type::u32 => cast_op![$lhs.0; $op; $rhs; u32; $ty],
            Type::i8  => cast_op![$lhs.0; $op; $rhs; i8 ; $ty],
            Type::i16 => cast_op![$lhs.0; $op; $rhs; i16; $ty],
            Type::i32 => cast_op![$lhs.0; $op; $rhs; i32; $ty],
        })
}

macro_rules! cast_ref_op {
    ($lhs:expr; $op:ident; $rhs:expr; $ty:ident; $tty:ty) =>
        (expr!((($lhs as $ty).$op(&($rhs as $ty))) as $tty));
}

macro_rules! sized_ref_op {
    ($lhs:ident $op:ident $rhs:expr; $ty:ty) =>
        (match $lhs.1 {
            Type::u8  => cast_ref_op![$lhs.0; $op; $rhs; u8 ; $ty],
            Type::u16 => cast_ref_op![$lhs.0; $op; $rhs; u16; $ty],
            Type::u32 => cast_ref_op![$lhs.0; $op; $rhs; u32; $ty],
            Type::i8  => cast_ref_op![$lhs.0; $op; $rhs; i8 ; $ty],
            Type::i16 => cast_ref_op![$lhs.0; $op; $rhs; i16; $ty],
            Type::i32 => cast_ref_op![$lhs.0; $op; $rhs; i32; $ty],
        })
}

macro_rules! cast_left_op {
    ($lhs:expr; $op:ident; $rhs:expr; $ty:ident; $tty:ty) =>
        (expr!((($lhs as $ty).$op($rhs)) as $tty));
}

macro_rules! sized_left_op {
    ($lhs:ident $op:ident $rhs:expr; $ty:ty) =>
        (match $lhs.1 {
            Type::u8  => cast_op![$lhs.0; $op; $rhs; u8 ; $ty],
            Type::u16 => cast_op![$lhs.0; $op; $rhs; u16; $ty],
            Type::u32 => cast_op![$lhs.0; $op; $rhs; u32; $ty],
            Type::i8  => cast_op![$lhs.0; $op; $rhs; i8 ; $ty],
            Type::i16 => cast_op![$lhs.0; $op; $rhs; i16; $ty],
            Type::i32 => cast_op![$lhs.0; $op; $rhs; i32; $ty],
        })
}

macro_rules! cast_left_ref_op {
    ($lhs:expr; $op:ident; $rhs:expr; $ty:ident; $tty:ty) =>
        (expr!((($lhs as $ty).$op(&$rhs)) as $tty));
}

macro_rules! sized_left_ref_op {
    ($lhs:ident $op:ident $rhs:expr; $ty:ty) =>
        (match $lhs.1 {
            Type::u8  => cast_op![$lhs.0; $op; $rhs; u8 ; $ty],
            Type::u16 => cast_op![$lhs.0; $op; $rhs; u16; $ty],
            Type::u32 => cast_op![$lhs.0; $op; $rhs; u32; $ty],
            Type::i8  => cast_op![$lhs.0; $op; $rhs; i8 ; $ty],
            Type::i16 => cast_op![$lhs.0; $op; $rhs; i16; $ty],
            Type::i32 => cast_op![$lhs.0; $op; $rhs; i32; $ty],
        })
}

macro_rules! partial_impl {
    ($ty:ty) => {
        impl PartialOrd<$ty> for Num {
            fn partial_cmp(&self, other: &$ty) -> Option<Ordering> {
                sized_ref_op![self partial_cmp *other; Option<Ordering>]
            }
        }

        impl PartialEq<$ty> for Num {
            fn eq(&self, other: &$ty) -> bool {
                sized_ref_op![self eq *other; bool]
            }
        }
    }
}

// partial_impl!{u8}
// partial_impl!{i8}
// partial_impl!{u16}
// partial_impl!{i16}
partial_impl!{u32}
partial_impl!{i32}

impl Num {
    pub fn _as(&self, ty: Type) -> Num {
        Num(cast_to![self, ty], ty)
    }

    pub fn size(&self) -> Size {
        match self.1 {
            Type::i8  | Type::u8  => Size::Size8,
            Type::i16 | Type::u16 => Size::Size16,
            Type::i32 | Type::u32 => Size::Size32
        }
    }

    pub fn low_byte_parity(&self) -> bool {
        (self.0 & 0xFF).count_ones() % 2 == 0
    }

    pub fn to_signed(&self) -> Num {
        match self.1 {
            Type::u8  => Num((self.0 as i8 ) as u32, Type::i8 ),
            Type::u16 => Num((self.0 as i16) as u32, Type::i16),
            Type::u32 => Num((self.0 as i32) as u32, Type::i32),
            _ => *self
        }
    }

    pub fn to_unsigned(&self) -> Num {
        match self.1 {
            Type::i8  => Num((self.0 as u8 ) as u32, Type::u8 ),
            Type::i16 => Num((self.0 as u16) as u32, Type::u16),
            Type::i32 => Num((self.0 as u32) as u32, Type::u32),
            _ => *self
        }
    }

    pub fn is_sign_bit_set(&self) -> bool {
        match self.1 {
            Type::i8  | Type::u8  => self.0 & 0xF0 != 0,
            Type::i16 | Type::u16 => self.0 & 0xF000 != 0,
            Type::i32 | Type::u32 => self.0 & 0xF0000000 != 0
        }
    }

    pub fn is_low_bit_set(&self) -> bool {
        self.0 & 0x1 != 0
    }

    pub fn overflowing_add(self, rhs: Num) -> (Num, bool)  {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        let x = sized_op![self overflowing_add rhs.0; (u32, bool)];
        (Num(x.0, self.1), x.1)
    }

    pub fn overflowing_sub(self, rhs: Num) -> (Num, bool)  {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        let x = sized_op![self overflowing_sub rhs.0; (u32, bool)];
        (Num(x.0, self.1), x.1)
    }

    pub fn overflowing_mul(self, rhs: Num) -> (Num, bool)  {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        let x = sized_op![self overflowing_mul rhs.0; (u32, bool)];
        (Num(x.0, self.1), x.1)
    }

    pub fn overflowing_div(self, rhs: Num) -> (Num, bool)  {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        let x = sized_op![self overflowing_div rhs.0; (u32, bool)];
        (Num(x.0, self.1), x.1)
    }

    pub fn overflowing_rem(self, rhs: Num) -> (Num, bool)  {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        let x = sized_op![self overflowing_rem rhs.0; (u32, bool)];
        (Num(x.0, self.1), x.1)
    }

}

impl PartialOrd<Num> for Num {
    fn partial_cmp(&self, other: &Num) -> Option<Ordering> {
        assert!(self.1 == other.1, "{:?} != {:?}", self.1, other.1);
        sized_ref_op![self partial_cmp other.0; Option<Ordering>]
    }
}

impl Ord for Num {
    fn cmp(&self, other: &Num) -> Ordering {
        assert!(self.1 == other.1, "{:?} != {:?}", self.1, other.1);
        sized_ref_op![self cmp other.0; Ordering]
    }
}

impl Add<Num> for Num {
    type Output = Num;

    fn add(self, rhs: Num) -> Num {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        Num(sized_op![self add rhs.0; u32], self.1)
    }
}

impl Sub<Num> for Num {
    type Output = Num;

    fn sub(self, rhs: Num) -> Num {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        Num(sized_op![self sub rhs.0; u32], self.1)
    }
}

impl Shl<Num> for Num {
    type Output = Num;

    fn shl(self, rhs: Num) -> Num {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        Num(sized_op![self shl rhs.0; u32], self.1)
    }
}

impl Shr<Num> for Num {
    type Output = Num;

    fn shr(self, rhs: Num) -> Num {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        Num(sized_op![self shr rhs.0; u32], self.1)
    }
}

impl Shl<usize> for Num {
    type Output = Num;

    fn shl(self, rhs: usize) -> Num {
        Num(sized_left_op![self shl rhs; u32], self.1)
    }
}

impl Shr<usize> for Num {
    type Output = Num;

    fn shr(self, rhs: usize) -> Num {
        Num(sized_left_op![self shr rhs; u32], self.1)
    }
}

impl BitAnd<Num> for Num {
    type Output = Num;

    fn bitand(self, rhs: Num) -> Num {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        Num(sized_op![self bitand rhs.0; u32], self.1)
    }
}

impl BitOr<Num> for Num {
    type Output = Num;

    fn bitor(self, rhs: Num) -> Num {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        Num(sized_op![self bitor rhs.0; u32], self.1)
    }
}

impl BitXor<Num> for Num {
    type Output = Num;

    fn bitxor(self, rhs: Num) -> Num {
        assert!(self.1 == rhs.1, "{:?} != {:?}", self.1, rhs.1);
        Num(sized_op![self bitxor rhs.0; u32], self.1)
    }
}

macro_rules! bit_impl {
    ($ty:ty) => {
        impl BitAnd<$ty> for Num {
            type Output = Num;

            fn bitand(self, rhs: $ty) -> Num {
                Num(sized_op![self bitand rhs; u32], self.1)
            }
        }

        impl BitOr<$ty> for Num {
            type Output = Num;

            fn bitor(self, rhs: $ty) -> Num {
                Num(sized_op![self bitor rhs; u32], self.1)
            }
        }

        impl BitXor<$ty> for Num {
            type Output = Num;

            fn bitxor(self, rhs: $ty) -> Num {
                Num(sized_op![self bitxor rhs; u32], self.1)
            }
        }

        impl Add<$ty> for Num {
            type Output = Num;

            fn add(self, rhs: $ty) -> Num {
                Num(sized_op![self add rhs; u32], self.1)
            }
        }

        impl Sub<$ty> for Num {
            type Output = Num;

            fn sub(self, rhs: $ty) -> Num {
                Num(sized_op![self sub rhs; u32], self.1)
            }
        }
    }
}

bit_impl!{u32}

impl LowerHex for Num {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.0.fmt(f)
    }
}

impl ToPrimitive for Num {
    fn to_u64(&self) -> Option<u64> {
        Some(cast![self, u64])
    }

    fn to_i64(&self) -> Option<i64> {
        Some(cast![self, i64])
    }
}


#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Type {
    u8,
    u16,
    u32,
    i8,
    i16,
    i32
}

enum_primitive! {
    #[derive(PartialEq, Eq, Debug, Clone, Copy)]
    pub enum Size {
        Size8  = 1,
        Size16 = 2,
        Size32 = 4,
        Size48 = 6
    }
}

impl Size {
    pub fn mask(self) -> u32 {
        0xFFFFFFFF >> (32 - (self as u32) * 8)
    }

    pub fn signed(self) -> Type {
        match self {
            Size::Size8  => Type::i8,
            Size::Size16 => Type::i16,
            Size::Size32 => Type::i32,
            _ => panic!("unsupported signed size")
        }
    }

    pub fn unsigned(self) -> Type {
        match self {
            Size::Size8  => Type::u8,
            Size::Size16 => Type::u16,
            Size::Size32 => Type::u32,
            _ => panic!("unsupported unsigned size")
        }
    }
}

impl Not for Size {
    type Output = Size;

    fn not(self) -> Size {
        match self {
            Size::Size16 => Size::Size32,
            Size::Size32 => Size::Size16,
            _ => self
        }
    }
}
