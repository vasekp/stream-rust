use crate::base::*;

pub(crate) fn unsign(num: Number) -> UNumber {
    num.into_parts().1
}
