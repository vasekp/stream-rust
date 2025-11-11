pub(crate) use num::*;

mod describe;
mod rules;
mod expr;
mod item;
mod node;
mod env;
mod error;
mod alphabet;
pub mod stop;

pub(crate) use rules::*;
pub use describe::*;
pub use expr::*;
pub use item::*;
pub use node::*;
pub use env::*;
pub use alphabet::*;
pub use error::*;

/// The base type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type Number = ibig::IBig;

/// The unsigned variant of the [`Number`] type.
pub type UNumber = ibig::UBig;


pub(crate) const CACHE_LEN: usize = 100;
