/// The base type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type Number = ibig::IBig;

/// The unsigned variant of the [`Number`] type.
pub type UNumber = ibig::UBig;

pub use ibig::ops::{DivRem, DivRemEuclid, RemEuclid, UnsignedAbs};

pub(crate) const CACHE_LEN: usize = 100;
