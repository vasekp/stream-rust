use crate::base::*;

/// The enum returned by [`Stream::len()`].
#[derive(Debug, Clone, PartialEq)]
pub enum Length {
    /// The length is known exactly, including empty streams.
    Exact(UNumber),
    /// The length has a known upper bound.
    AtMost(UNumber),
    /// The stream is known to be infinite.
    Infinite,
    /// The length is not known but promises to be finite.
    UnknownFinite,
    /// Nothing can be inferred about the length.
    Unknown
}

impl Length {
    pub fn at_most(value: Length) -> Length {
        use Length::*;
        match value {
            Exact(x) => AtMost(x),
            AtMost(x) => AtMost(x),
            UnknownFinite => UnknownFinite,
            _ => Unknown
        }
    }

    pub fn possibly_eq(l1: &Length, l2: &Length) -> bool {
        use Length::*;
        match (l1, l2) {
            (Unknown, _) | (_, Unknown) => true,
            (Infinite, Infinite) => true,
            (Infinite, _) | (_, Infinite) => false,
            (Exact(x), Exact(y)) => x == y,
            (Exact(x), AtMost(y)) => x <= y,
            (AtMost(x), Exact(y)) => y <= x,
            _ => true
        }
    }

    pub fn intersection(l1: Length, l2: Length) -> Length {
        use Length::*;
        match (l1, l2) {
            (Infinite, len) | (len, Infinite) => len,
            (Unknown, len) | (len, Unknown) => Length::at_most(len),
            // can't be merged with previous, otherwise (UnkFin, Unk) would give at_most(Unk) == Unk
            (UnknownFinite, len) | (len, UnknownFinite) => Length::at_most(len),
            (Exact(x), Exact(y)) => Exact(std::cmp::min(x, y)),
            (Exact(x) | AtMost(x), Exact(y) | AtMost(y)) => AtMost(std::cmp::min(x, y))
        }
    }

    pub fn map<F: FnOnce(&UNumber) -> UNumber>(&self, f: F) -> Length {
        use Length::*;
        match self {
            Exact(len) => Exact(f(len)),
            AtMost(len) => AtMost(f(len)),
            UnknownFinite => UnknownFinite,
            Unknown => Unknown,
            Infinite => Infinite
        }
    }
}

impl<T> From<T> for Length where T: Into<UNumber> {
    fn from(value: T) -> Self {
        Length::Exact(value.into())
    }
}

impl std::ops::Add for Length {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        use Length::*;
        match (self, rhs) {
            (_, Infinite) | (Infinite, _) => Infinite,
            (_, Unknown) | (Unknown, _) => Unknown,
            (_, UnknownFinite) | (UnknownFinite, _) => UnknownFinite,
            (Exact(a), Exact(b)) => Exact(a + b),
            (Exact(a) | AtMost(a), Exact(b) | AtMost(b)) => AtMost(a + b)
        }
    }
}

impl<T: std::borrow::Borrow<UNumber>> std::ops::AddAssign<T> for Length {
    fn add_assign(&mut self, rhs: T) {
        use Length::*;
        match self {
            Exact(len) | AtMost(len) => *len += rhs.borrow(),
            _ => ()
        }
    }
}

impl<T: std::borrow::Borrow<UNumber>> std::ops::Add<T> for Length {
    type Output = Self;

    fn add(mut self, rhs: T) -> Self {
        self += rhs.borrow();
        self
    }
}
