use crate::base::*;

fn eval_remove(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_args_nonempty()?;
    let stm = node.source_checked()?.as_stream()?;
    let index = node.only_arg_checked()?
        .as_num()?
        .try_cast_within(UNumber::one()..)?;
    Ok(Item::new_stream(Remove{
        head: node.head.clone(),
        source: Rc::clone(stm),
        index,
    }))
}

struct Remove {
    head: Head,
    source: Rc<dyn Stream>,
    index: UNumber,
}

impl Describe for Remove {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.index)
            .finish(prec)
    }
}

impl Stream for Remove {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        RemoveIter{
            index: UNumber::one(),
            iter: self.source.iter(),
            node: self
        }.wrap()
    }

    fn len(&self) -> Length {
        let slen = self.source.len();
        match &slen {
            Length::Exact(len) if len >= &self.index => Length::Exact(len - 1),
            Length::Exact(_) | Length::AtMost(_) | Length::UnknownFinite
                | Length::Unknown | Length::Infinite => slen
        }
    }
}

struct RemoveIter {
    node: Rc<Remove>,
    iter: Box<dyn SIterator>,
    index: UNumber,
}

impl PreIterator for RemoveIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.index == self.node.index {
            iter_try!(self.iter.next());
        }
        self.index += 1;
        self.iter.next()
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        if self.index > self.node.index || &self.index + n <= self.node.index {
            self.index += n;
            self.iter.advance(n)
        } else {
            let diff = &self.node.index - &self.index;
            let after = n - &diff;
            if let Some(remain) = self.iter.advance(&diff)? {
                return Ok(Some(remain + after));
            }
            if after.is_zero() {
                return Ok(None);
            }
            if self.iter.next()?.is_none() {
                return Ok(Some(after));
            }
            self.index = &self.node.index + &after;
            self.iter.advance(&after)
        }
    }

    fn origin(&self) -> &Rc<Remove> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remove() {
        for modif in ["", ".$lenUU", ".$lenAM"] {
            test_eval!(&format!("(1..5){modif}.remove(1)") => "[2, 3, 4, 5]");
            test_eval!(&format!("(1..5){modif}.remove(2)") => "[1, 3, 4, 5]");
            test_eval!(&format!("(1..5){modif}.remove(3)") => "[1, 2, 4, 5]");
            test_eval!(&format!("(1..5){modif}.remove(4)") => "[1, 2, 3, 5]");
            test_eval!(&format!("(1..5){modif}.remove(5)") => "[1, 2, 3, 4]");
            test_eval!(&format!("(1..5){modif}.remove(6)") => "[1, 2, 3, 4, 5]");
            test_len!(&format!("(1..5){modif}.remove(1)") => 4);
            test_len!(&format!("(1..5){modif}.remove(2)") => 4);
            test_len!(&format!("(1..5){modif}.remove(3)") => 4);
            test_len!(&format!("(1..5){modif}.remove(4)") => 4);
            test_len!(&format!("(1..5){modif}.remove(5)") => 4);
            test_len!(&format!("(1..5){modif}.remove(6)") => 5);
            test_advance(&format!("(1..5){modif}.remove(1)"));
            test_advance(&format!("(1..5){modif}.remove(2)"));
            test_advance(&format!("(1..5){modif}.remove(3)"));
            test_advance(&format!("(1..5){modif}.remove(4)"));
            test_advance(&format!("(1..5){modif}.remove(5)"));
            test_advance(&format!("(1..5){modif}.remove(6)"));
        }
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("remove", eval_remove, r#"
Returns `stream` with item at position `index` removed.
If the stream has less than `index` items, it is unchanged.
= stream.?(index)
> (1..5).?(3) => [1, 2, 4, 5]
: replace
: append
: prepend
"#);
}
