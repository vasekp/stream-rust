pub(crate) use num::*;

mod rules;
mod expr;
mod item;
mod node;
mod env;
mod error;
mod alphabet;
pub mod stop;

pub(crate) use rules::*;
pub use expr::*;
pub use item::*;
pub use node::*;
pub use env::*;
pub use alphabet::*;
pub use error::*;

/// The base type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type Number = num::BigInt;

/// The unsigned variant of the [`Number`] type.
pub type UNumber = num::BigUint;


pub(crate) const CACHE_LEN: usize = 100;


/// A trait for the ability to turn a Stream language object (notably, [`Expr`]) into an input form.
pub trait Describe {
    /// Construct a string representation of `self`. This is meant for storing object across
    /// sessions. The resulting `String` must be a syntactically valid input that reconstruct a
    /// copy of the original object on [`parser::parse()`](crate::parser::parse()) and
    /// [`Expr::eval()`].
    fn describe(&self) -> String {
        self.describe_prec(0)
    }

    fn describe_prec(&self, prec: u32) -> String;
}

impl Describe for Number {
    fn describe_prec(&self, prec: u32) -> String {
        if prec > 0 && self.is_negative() {
            format!("({})", self)
        } else {
            self.to_string()
        }
    }
}

impl Describe for UNumber {
    fn describe_prec(&self, _: u32) -> String {
        self.to_string()
    }
}
