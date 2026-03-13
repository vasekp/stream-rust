use crate::base::*;

#[derive(Clone)]
pub struct List<I: ItemType>(Vec<I>);

pub type LiteralString = List<Char>;

impl<I: ItemType> List<I> where List<I>: Describe {
    pub fn iter(self: &Rc<Self>) -> Box<dyn SIterator<I>> {
        Rc::clone(self).to_iter()
    }
}

impl<I: ItemType> Stream<I> for List<I> where List<I>: Describe {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        Box::new(ListIter::new(self))
    }

    fn len(&self) -> Length {
        Length::from(self.0.len())
    }
}

impl Describe for List<Item> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&Head::Lang(LangItem::List), env)
            .push_args(&self.0)
            .finish(prec)
    }
}

impl Describe for List<Char> {
    fn describe_inner(&self, _prec: u32, _env: &Env) -> String {
        let mut ret = String::new();
        ret.push('"');
        for ch in &self.0 {
            ret += &format!("{ch:#}");
        }
        ret.push('"');
        ret
    }
}

impl<I: ItemType> From<Vec<I>> for List<I> {
    fn from(vec: Vec<I>) -> Self {
        List(vec)
    }
}

impl From<&str> for List<Char> {
    fn from(s: &str) -> Self {
        List(s.chars().map(Char::from).collect())
    }
}

impl List<Char> {
    pub fn as_slice(&self) -> &[Char] {
        &self.0[..]
    }
}

struct ListIter<I: ItemType> {
    node: Rc<List<I>>,
    index: usize,
}

impl<I: ItemType> ListIter<I> {
    fn new(node: Rc<List<I>>) -> Self {
        Self{node, index: 0}
    }

    fn len_remain_impl(&self) -> usize {
        self.node.0.len() - self.index
    }
}

impl<I: ItemType> SIterator<I> for ListIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        let val = self.node.0.get(self.index);
        self.index += 1;
        Ok(val.cloned())
    }

    fn len_remain(&self) -> Length {
        Length::from(self.len_remain_impl())
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        match usize::try_from(&n) {
            Ok(n) if n <= self.len_remain_impl() => {
                self.index += n;
                Ok(None)
            },
            _ => {
                Ok(Some(n - self.len_remain_impl()))
            }
        }
    }
}
