use crate::base::*;

#[derive(Clone)]
pub struct List {
    pub(crate) vec: Vec<Item>,
    pub(crate) is_string: TriState
}

impl Stream for List {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(self.vec.iter().map(|x| Ok(x.clone())))
    }

    fn length(&self) -> Length {
        Length::from(self.vec.len())
    }

    fn is_string(&self) -> TriState {
        self.is_string
    }
}

impl Describe for List {
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&Head::Lang(LangItem::List), None::<&Item>, &self.vec, prec)
    }
}

impl From<Vec<Item>> for List {
    fn from(vec: Vec<Item>) -> List {
        List { vec, is_string: false.into() }
    }
}
