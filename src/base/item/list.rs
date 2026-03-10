use crate::base::*;

#[derive(Clone)]
pub struct List(Vec<Item>);

impl List {
    pub fn iter<'node>(&'node self) -> Box<dyn SIterator<Item> + 'node> {
        Box::new(self.0.iter().map(|x| Ok(x.clone())))
    }
}

impl Stream for List {
    fn iter<'node>(&'node self) -> Result<Box<dyn SIterator<Item> + 'node>, StreamError> {
        Ok(self.iter())
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
