use crate::base::*;

fn eval_ddup(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let stm = node.source_checked()?.to_stream()?;
    Ok(Item::new_stream(DDup{head: node.head.clone(), source: stm}))
}

struct DDup {
    head: Head,
    source: Rc<dyn Stream>,
}


impl Describe for DDup {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for DDup {
    fn iter<'node>(&'node self) -> Result<Box<dyn SIterator + 'node>, StreamError> {
        Ok(Box::new(DDupIter{iter: self.source.iter(), seen: vec![]}))
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct DDupIter<'node> {
    iter: Box<dyn SIterator + 'node>,
    seen: Vec<Item>
}

impl SIterator for DDupIter<'_> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        'a: loop {
            check_stop!();
            let item = iter_try!(self.iter.next());
            for seen in &self.seen {
                if item.try_eq(seen)? {
                    continue 'a;
                }
            }
            self.seen.push(item.clone());
            return Ok(Some(item));
        }
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.iter.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ddup() {
        use super::*;
        test_eval!("seq.ddup" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("(seq/5).ddup" => "[0, 1, 2, 3, 4, ...]");
        test_eval!("(1..3):{1..#}.flatten.ddup" => "[1, 2, 3]");
        test_eval!("(1..3).repeat(5).ddup" => "[1, 2, 3]");
        test_len!("[]" => 0);
        test_describe!("seq.ddup" => "seq.ddup");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("ddup", eval_ddup, r#"
Keeps only the first appearance of any repeated item in `stream`.
= stream.?
> [1, 2, 1, 2, 3].? => [1, 2, 3]
> "abracadabra".?chars.?.?string => "abrcd"
"#);
}
