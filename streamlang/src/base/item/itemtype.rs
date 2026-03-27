use crate::base::*;

pub trait ItemType: Clone + Describe + Into<Item> + 'static {
    fn from_vec(vec: Vec<Self>) -> Item;
    fn from_rc(stm: Rc<dyn Stream<Self>>) -> Item;
    fn try_eq(&self, other: &Self) -> SResult<bool>;
    fn type_name() -> &'static str;

    fn to_item(&self) -> Item { self.clone().into() }
}

impl ItemType for Item {
    fn type_name() -> &'static str { "stream" }

    fn from_vec(vec: Vec<Item>) -> Item {
        Item::new_stream(List::from(vec))
    }

    fn from_rc(stm: Rc<dyn Stream<Item>>) -> Item {
        Item::Stream(stm)
    }

    fn try_eq(&self, other: &Item) -> SResult<bool> {
        self.try_eq(other)
    }
}

impl ItemType for Char {
    fn type_name() -> &'static str { "string" }

    fn from_vec(vec: Vec<Char>) -> Item {
        Item::new_string(LiteralString::from(vec))
    }

    fn from_rc(stm: Rc<dyn Stream<Char>>) -> Item {
        Item::String(stm)
    }

    fn try_eq(&self, other: &Char) -> SResult<bool> {
        Ok(self == other)
    }
}
