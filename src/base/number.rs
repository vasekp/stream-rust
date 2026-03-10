use crate::base::error::*;
use num::Signed;

/// The base type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type Number = ibig::IBig;

/// The unsigned variant of the [`Number`] type.
pub type UNumber = ibig::UBig;

pub use ibig::ops::{DivRem, DivRemEuclid, RemEuclid, UnsignedAbs};

pub(crate) const CACHE_LEN: usize = 100;

pub(crate) trait TryCast<T>: IsSigned + Ord + num::Zero where T: IsSigned + for<'a> TryFrom<&'a Self> {
    fn try_cast(&self) -> Result<T, StreamError> {
        if Self::IS_SIGNED && !T::IS_SIGNED && self < &Self::zero() {
            Err("value can't be negative".into())
        } else {
            self.try_into().map_err(|_| "value too large".into())
        }
    }

    fn try_cast_within(&self, range: impl std::ops::RangeBounds<T>) -> Result<T, StreamError>
    where T: Ord + std::fmt::Display {
        if Self::IS_SIGNED && !T::IS_SIGNED && self < &Self::zero() {
            Err("value can't be negative".into())
        } else {
            let x = self.try_into().map_err(|_| "value too large")?;
            if range.contains(&x) {
                Ok(x)
            } else {
                use std::ops::Bound::*;
                let err = match (range.start_bound(), range.end_bound()) {
                    (Included(min), Unbounded) => format!("value must be at least {min}"),
                    (Unbounded, Included(max)) => format!("value must be at most {max}"),
                    (Included(min), Included(max)) => format!("value must be between {min} and {max}"),
                    _ => "value out of range".into()
                };
                Err(err.into())
            }
        }
    }
}

pub(crate) trait IsSigned {
    const IS_SIGNED: bool;
}

macro_rules! impl_is_signed {
    ($($t:ty => $v:expr),* $(,)?) => {
        $(impl IsSigned for $t {
            const IS_SIGNED: bool = $v;
        })*
    };
}

impl_is_signed!(
    i8  => true,  i16 => true,  i32 => true,  i64 => true,  i128 => true,  isize => true,
    u8  => false, u16 => false, u32 => false, u64 => false, u128 => false, usize => false,
    Number => true, UNumber => false,
);

impl TryCast<u32> for Number { }
impl TryCast<usize> for Number { }
impl TryCast<i32> for Number { }
impl TryCast<UNumber> for Number { }
impl TryCast<u32> for UNumber { }
impl TryCast<usize> for UNumber { }

pub(crate) trait TryUnsign {
    type Unsigned;

    fn try_unsign(&self) -> Result<Self::Unsigned, StreamError>;
}

impl TryUnsign for Number {
    type Unsigned = UNumber;

    fn try_unsign(&self) -> Result<Self::Unsigned, StreamError> {
        if self.is_negative() {
            Err("value can't be negative".into())
        } else {
            Ok(self.unsigned_abs())
        }
    }
}
