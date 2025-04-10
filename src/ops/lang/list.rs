use crate::base::*;

#[derive(Clone)]
struct List(Vec<Item>);

impl List {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source()?);
        Ok(Item::new_stream(List::from(node.args)))
    }
}

impl Stream for List {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(self.0.iter().map(|x| Ok(x.clone())))
    }

    fn length(&self) -> Length {
        Length::from(self.0.len())
    }
}

impl Describe for List {
    fn describe(&self, prec: u32) -> String {
        Node::describe_helper(&Head::Lang(LangItem::List), None::<&Item>, &self.0, prec)
    }
}

impl From<Vec<Item>> for List {
    fn from(vec: Vec<Item>) -> List {
        List(vec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list() {
        use crate::parser::parse;
        assert_eq!(parse("[1,2,3]").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
        test_len_exact(&parse("[1,2,3]").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("[1]").unwrap().eval().unwrap(), 1);
        test_len_exact(&parse("[]").unwrap().eval().unwrap(), 0);
        test_skip_n(&parse("[1,2,3]").unwrap().eval().unwrap());
        test_skip_n(&parse("[1]").unwrap().eval().unwrap());
        test_skip_n(&parse("[]").unwrap().eval().unwrap());
        assert_eq!(parse("[1,2,3]").unwrap().eval().unwrap().describe(0), "[1, 2, 3]");
        assert_eq!(parse("[]").unwrap().eval().unwrap().describe(0), "[]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("$list", List::eval);
}
