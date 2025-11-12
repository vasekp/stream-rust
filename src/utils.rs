use crate::base::*;

pub(crate) fn unsign(num: Number) -> UNumber {
    num.abs().try_into().expect("must be ≥ 0 after abs()")
}
