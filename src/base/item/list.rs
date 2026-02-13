use crate::base::*;

#[derive(Clone)]
pub struct List(Vec<Item>);

impl Stream for List {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(self.0.iter().map(|x| Ok(x.clone())))
    }

    fn len(&self) -> Length {
        Length::from(self.0.len())
    }
}

impl Describe for List {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&Head::Lang(LangItem::List), env)
            .push_args(&self.0)
            .finish(prec)
    }
}

impl From<Vec<Item>> for List {
    fn from(vec: Vec<Item>) -> List {
        List(vec)
    }
}
