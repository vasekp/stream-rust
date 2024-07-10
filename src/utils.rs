use std::ops::RangeBounds;
use std::ops::Bound::*;
use crate::base::{Number, BaseError};

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

pub(crate) trait NumWithin : PartialOrd {
    fn check_within(&self, range: impl RangeBounds<Self>) -> Result<(), BaseError>;
}

impl NumWithin for Number {
    fn check_within(&self, range: impl RangeBounds<Self>) -> Result<(), BaseError> {
        match range.contains(self) {
            true => Ok(()),
            false => Err(format!("expected {}, found {}", describe_range(&range), &self).into())
        }
    }
}
