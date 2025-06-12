use crate::base::*;

fn eval_ddup(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match rnode {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero }
            => Ok(Item::new_stream(DDup{head, source: stm.into()})),
        _ => Err(StreamError::new("expected: stream.ddup", rnode))
    }
}

#[derive(Clone)]
struct DDup {
    head: Head,
    source: BoxedStream,
}


impl Describe for DDup {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), None::<&Item>, prec, env)
    }
}

impl Stream for DDup {
    fn iter(&self) -> Box<dyn SIterator + '_> {
        Box::new(DDupIter{iter: self.source.iter(), seen: vec![]})
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct DDupIter<'node> {
    iter: Box<dyn SIterator + 'node>,
    seen: Vec<Item>
}

impl Iterator for DDupIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        'a: loop {
            check_stop!(iter);
            let item = iter_try_expr!(self.iter.next()?);
            for seen in &self.seen {
                if iter_try_expr!(item.try_eq(seen)) {
                    continue 'a;
                }
            }
            self.seen.push(item.clone());
            return Some(Ok(item));
        }
    }
}

impl SIterator for DDupIter<'_> {
    fn len_remain(&self) -> Length {
        Length::at_most(self.iter.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ddup() {
        use super::*;
        use crate::parser::parse;

        test_eval!("seq.ddup" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("(seq/5).ddup" => "[0, 1, 2, 3, 4, ...]");
        test_eval!("(1..3):{1..#}.flatten.ddup" => "[1, 2, 3]");
        test_eval!("(1..3).repeat(5).ddup" => "[1, 2, 3]");
        test_len!("[]" => 0);
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("ddup", eval_ddup);
}
