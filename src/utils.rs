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

    fn is_string(&self) -> bool {
        true
    }
}

impl Describe for EmptyString {
    fn describe(&self) -> String {
        "\"\"".into()
    }
}
