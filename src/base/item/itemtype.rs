use crate::base::*;

pub trait ItemType: Clone + Describe + Into<Item> + 'static {
    fn from_vec(vec: Vec<Self>) -> Item;
    fn from_box(stm: Box<dyn Stream<Self>>) -> Item;
    fn listout(stm: &(dyn Stream<Self> + 'static)) -> Result<Vec<Self>, StreamError>;
    fn try_eq(&self, other: &Self) -> Result<bool, StreamError>;
}

impl ItemType for Item {
    fn from_vec(vec: Vec<Item>) -> Item {
        Item::Stream(Box::new(List::from(vec)))
    }

    fn from_box(stm: Box<dyn Stream<Item>>) -> Item {
        Item::Stream(stm)
    }

    fn listout(stm: &(dyn Stream<Self> + 'static)) -> Result<Vec<Self>, StreamError> {
        stm.listout_impl()
    }

    fn try_eq(&self, other: &Item) -> Result<bool, StreamError> {
        self.try_eq(other)
    }
}

impl ItemType for Char {
    fn from_vec(vec: Vec<Char>) -> Item {
        Item::String(Box::new(LiteralString::from(vec)))
    }

    fn from_box(stm: Box<dyn Stream<Char>>) -> Item {
        Item::String(stm)
    }

    fn listout(stm: &(dyn Stream<Self> + 'static)) -> Result<Vec<Self>, StreamError> {
        stm.listout_impl()
    }

    fn try_eq(&self, other: &Char) -> Result<bool, StreamError> {
        Ok(self == other)
    }
}
