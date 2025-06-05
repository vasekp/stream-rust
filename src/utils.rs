use crate::base::*;

pub(crate) fn unsign(num: Number) -> UNumber {
    num.into_parts().1
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TriState {
    True,
    False,
    Either
}

impl TriState {
    #[allow(dead_code)]
    pub fn is_true(self) -> bool {
        self == Self::True
    }

    #[allow(dead_code)]
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
