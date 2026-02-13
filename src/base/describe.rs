use super::*;

/// A trait for the ability to turn a Stream language object (notably, [`Expr`]) into an input form.
pub trait Describe {
    /// Construct a string representation of `self`. This is meant for storing object across
    /// sessions. The resulting `String` must be a syntactically valid input that reconstruct a
    /// copy of the original object on [`parser::parse()`](crate::parser::parse()) and
    /// [`Expr::eval()`].
    fn describe(&self) -> String {
        self.describe_inner(0, &Default::default())
    }

    fn describe_inner(&self, prec: u32, env: &Env) -> String;
}

macro_rules! impl_describe_signed {
    ($t: ty) => {
        impl Describe for $t {
            fn describe_inner(&self, prec: u32, _env: &Env) -> String {
                if prec > 0 && self.is_negative() {
                    format!("({})", self)
                } else {
                    self.to_string()
                }
            }
        }
    }
}

macro_rules! impl_describe_unsigned {
    ($t: ty) => {
        impl Describe for $t {
            fn describe_inner(&self, _prec: u32, _env: &Env) -> String {
                self.to_string()
            }
        }
    }
}

impl_describe_signed!(Number);
impl_describe_signed!(i32);

impl_describe_unsigned!(UNumber);
impl_describe_unsigned!(u32);
impl_describe_unsigned!(usize);

impl<T: Describe> Describe for &T {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        (**self).describe_inner(prec, env)
    }
}
