use crate::base::*;

fn eval_replace(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_args_nonempty()?;
    let stm = node.source_checked()?.as_stream()?;
    let (index, item) = match &node.args[..] {
        [Item::Number(ix), item] => (ix.try_cast_within(UNumber::one()..)?, item.clone()),
        _ => return Err(StreamError::usage(&node.head)),
    };
    Ok(Item::new_stream(Replace{
        head: node.head.clone(),
        source: Rc::clone(stm),
        index, item,
    }))
}

struct Replace {
    head: Head,
    source: Rc<dyn Stream>,
    index: UNumber,
    item: Item,
}

impl Describe for Replace {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.index)
            .push_arg(&self.item)
            .finish(prec)
    }
}

impl Stream for Replace {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        ReplaceIter{
            index: UNumber::one(),
            iter: self.source.iter(),
            node: self
        }.wrap()
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

struct ReplaceIter {
    node: Rc<Replace>,
    iter: Box<dyn SIterator>,
    index: UNumber,
}

impl PreIterator for ReplaceIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let mut item = iter_try!(self.iter.next());
        if self.index == self.node.index {
            item = self.node.item.clone();
        }
        self.index += 1;
        Ok(Some(item))
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        self.index += &n;
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain()
    }

    fn origin(&self) -> &Rc<Replace> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_replace() {
        test_eval!("(1..5).replace(1, 'a')" => "['a', 2, 3, 4, 5]");
        test_eval!("(1..5).replace(2, 'a')" => "[1, 'a', 3, 4, 5]");
        test_eval!("(1..5).replace(5, 'a')" => "[1, 2, 3, 4, 'a']");
        test_eval!("(1..5).replace(6, 'a')" => "[1, 2, 3, 4, 5]");
        test_eval!("(1..5).replace(0, 'a')" => err);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("replace", eval_replace, r#"
Returns `stream` with item at position `index` replaced by `item`.
If the stream has less than `index` items, it is unchanged.
= stream.?(index, item)
> (1..5).?(3, "a") => [1, 2, "a", 4, 5]
: subst
: append
: prepend
: remove
"#);
}
