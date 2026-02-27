use crate::base::*;

pub trait ItemType: Clone + Describe + Into<Item> + 'static {
    fn from_vec(vec: Vec<Self>) -> Item;
    fn from_rc(stm: Rc<dyn Stream<Self>>) -> Item;
    fn listout(stm: &Rc<dyn Stream<Self>>) -> Result<Vec<Self>, StreamError>;
    fn try_eq(&self, other: &Self) -> Result<bool, StreamError>;
}

impl ItemType for Item {
    fn from_vec(vec: Vec<Item>) -> Item {
        Item::new_stream(List::from(vec))
    }

    fn from_rc(stm: Rc<dyn Stream<Item>>) -> Item {
        Item::Stream(stm)
    }

    fn listout(stm: &Rc<dyn Stream<Self>>) -> Result<Vec<Self>, StreamError> {
        stm.listout_impl()
    }

    fn try_eq(&self, other: &Item) -> Result<bool, StreamError> {
        self.try_eq(other)
    }
}

impl ItemType for Char {
    fn from_vec(vec: Vec<Char>) -> Item {
        Item::new_string(LiteralString::from(vec))
    }

    fn from_rc(stm: Rc<dyn Stream<Char>>) -> Item {
        Item::String(stm)
    }

    fn listout(stm: &Rc<dyn Stream<Self>>) -> Result<Vec<Self>, StreamError> {
        stm.listout_impl()
    }

    fn try_eq(&self, other: &Char) -> Result<bool, StreamError> {
        Ok(self == other)
    }
}
