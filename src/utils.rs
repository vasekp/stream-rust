use std::ops::RangeBounds;
use std::ops::Bound::*;
use crate::base::*;

fn describe_range<T>(range: &impl RangeBounds<T>) -> String
where T: std::fmt::Display + std::fmt::Debug + PartialEq {
    match (range.start_bound(), range.end_bound()) {
        (Included(min), Included(max)) if min == max => format!("exactly {min}"),
        (Included(min), Included(max)) => format!("between {min} and {max}"),
        (Included(min), Unbounded) => format!("at least {min}"),
        (Unbounded, Included(max)) => format!("at most {max}"),
        _ => panic!("describe_range: bounds {:?}, {:?}", range.start_bound(), range.end_bound())
    }
}

pub(crate) trait NumWithin: PartialOrd + std::fmt::Display + std::fmt::Debug + Sized {
    fn check_within(&self, range: impl RangeBounds<Self>) -> Result<(), BaseError> {
        match range.contains(self) {
            true => Ok(()),
            false => Err(format!("expected {}, found {}", describe_range(&range), &self).into())
        }
    }
}

impl NumWithin for Number {}
impl NumWithin for UNumber {}


pub(crate) fn unsign(num: Number) -> UNumber {
    num.into_parts().1
}


#[derive(Clone)]
pub(crate) struct EmptyStream();

impl Stream for EmptyStream {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(std::iter::empty())
    }
}

impl Describe for EmptyStream {
    fn describe(&self) -> String {
        "[]".into()
    }
}


#[derive(Clone)]
pub struct EmptyString();

impl Stream for EmptyString {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(std::iter::empty())
    }

    fn is_string(&self) -> TriState {
        TriState::True
    }
}

impl Describe for EmptyString {
    fn describe(&self) -> String {
        "\"\"".into()
    }
}


#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TriState {
    True,
    False,
    Either
}

impl TriState {
    pub fn is_true(self) -> bool {
        self == Self::True
    }

    pub fn can_be_true(self) -> bool {
        self != Self::False
    }

    #[allow(clippy::result_unit_err)]
    pub fn join(self, other: TriState) -> Result<TriState, ()> {
        use TriState::*;
        match (self, other) {
            (True, True | Either) => Ok(True),
            (False, False | Either) => Ok(False),
            (Either, x) => Ok(x),
            _ => Err(())
        }
    }
}

impl From<bool> for TriState {
    fn from(val: bool) -> Self {
        if val { Self::True } else { Self::False }
    }
}
