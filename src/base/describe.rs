use super::*;

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

impl<T: Describe> Describe for &T {
    fn describe_prec(&self, prec: u32) -> String {
        (**self).describe_prec(prec)
    }
}
