use crate::base::*;

pub(crate) fn unsign(num: Number) -> UNumber {
    num.into_parts().1
}

pub(crate) fn factorial(x: u32) -> UNumber {
    if x < 2 { UNumber::one() } else { range_prod(2, x) }
}

fn range_prod(from: u32, to: u32) -> UNumber {
    match to - from {
        0 => UNumber::from(from),
        1 => UNumber::from(from) * (from + 1),
        _ => {
            let mid = from + (to - from) / 2;
            range_prod(from, mid) * range_prod(mid + 1, to)
        }
    }
}

#[cfg(test)]
#[test]
fn test_factorial() {
    let mut r = 1u64;
    assert_eq!(factorial(0), 1u32.into());
    for x in 1..20 {
        r *= x as u64;
        assert_eq!(factorial(x), r.into());
    }
    assert_eq!(factorial(100), "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000".parse().unwrap());
}
