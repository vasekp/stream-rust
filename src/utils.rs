use std::ops::RangeBounds;
use std::ops::Bound::*;

pub(crate) fn describe_range<T>(range: &impl RangeBounds<T>) -> String
where T: std::fmt::Display + std::fmt::Debug + PartialEq {
    match (range.start_bound(), range.end_bound()) {
        (Included(min), Included(max)) if min == max => format!("exactly {min}"),
        (Included(min), Included(max)) => format!("between {min} and {max}"),
        (Included(min), Unbounded) => format!("at least {min}"),
        (Unbounded, Included(max)) => format!("at most {max}"),
        _ => panic!("describe_range: bounds {:?}, {:?}", range.start_bound(), range.end_bound())
    }
}
