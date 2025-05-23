use crate::base::*;

pub(crate) trait Checks {
    type Element;

    fn source(&self) -> Option<&Self::Element>;
    //fn source_mut(&mut self) -> Option<&mut Self::Element>;
    fn args(&self) -> &Vec<Self::Element>;
    //fn args_mut(&mut self) -> &mut Vec<Self::Element>;

    fn check_no_source(&self) -> Result<(), BaseError> {
        match &self.source() {
            Some(_) => Err("no source accepted".into()),
            None => Ok(())
        }
    }

    fn source_checked(&self) -> Result<&Self::Element, BaseError> {
        self.source().ok_or("source required".into())
    }

    fn check_no_args(&self) -> Result<(), BaseError> {
        if !self.args().is_empty() {
            Err("no arguments expected".into())
        } else {
            Ok(())
        }
    }

    fn check_args_nonempty(&self) -> Result<(), BaseError> {
        if self.args().is_empty() {
            Err("at least 1 argument required".into())
        } else {
            Ok(())
        }
    }

    fn first_arg_checked(&self) -> Result<&Self::Element, BaseError> {
        self.args().get(0).ok_or("at least 1 argument required".into())
    }

    /*fn first_arg_checked_mut(&mut self) -> Result<&mut Self::Element, BaseError> {
        self.args_mut().get_mut(0).ok_or("at least 1 argument required".into())
    }*/
}

impl Checks for Node {
    type Element = Expr;

    fn source(&self) -> Option<&Expr> { self.source.as_deref() }
    //fn source_mut(&mut self) -> Option<&mut Expr> { self.source.as_deref_mut() }
    fn args(&self) -> &Vec<Expr> { &self.args }
    //fn args_mut(&mut self) -> &mut Vec<Expr> { &mut self.args }
}

impl Checks for ENode {
    type Element = Item;

    fn source(&self) -> Option<&Item> { self.source.as_ref() }
    //fn source_mut(&mut self) -> Option<&mut Item> { self.source.as_mut() }
    fn args(&self) -> &Vec<Item> { &self.args }
    //fn args_mut(&mut self) -> &mut Vec<Item> { &mut self.args }
}

