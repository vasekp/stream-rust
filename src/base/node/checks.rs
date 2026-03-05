use crate::base::*;

pub(crate) trait Checks {
    type Element;

    fn source(&self) -> Option<&Self::Element>;
    fn args(&self) -> &Vec<Self::Element>;

    fn check_source(&self) -> Result<(), StreamError> {
        match &self.source() {
            Some(_) => Ok(()),
            None => Err(StreamError::new0("source required")),
        }
    }

    fn check_no_source(&self) -> Result<(), StreamError> {
        match &self.source() {
            Some(_) => Err(StreamError::new0("no source accepted")),
            None => Ok(())
        }
    }

    #[allow(unused)]
    fn source_checked(&self) -> Result<&Self::Element, StreamError> {
        self.source().ok_or(StreamError::new0("source required"))
    }

    fn check_no_args(&self) -> Result<(), StreamError> {
        if !self.args().is_empty() {
            Err(StreamError::new0("no arguments expected"))
        } else {
            Ok(())
        }
    }

    fn check_args_nonempty(&self) -> Result<(), StreamError> {
        if self.args().is_empty() {
            Err(StreamError::new0("at least 1 argument required"))
        } else {
            Ok(())
        }
    }

    fn first_arg_checked(&self) -> Result<&Self::Element, StreamError> {
        self.args().first().ok_or(StreamError::new0("at least 1 argument required"))
    }
}

impl Checks for Node {
    type Element = Expr;

    fn source(&self) -> Option<&Expr> { self.source.as_ref() }
    fn args(&self) -> &Vec<Expr> { &self.args }
}

impl Checks for ENode {
    type Element = Item;

    fn source(&self) -> Option<&Item> { self.source.as_ref() }
    fn args(&self) -> &Vec<Item> { &self.args }
}

