use crate::base::*;

fn eval_drep(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let stm = node.source_checked()?.to_stream()?;
    Ok(Item::new_stream(DRep{head: node.head.clone(), source: stm}))
}

struct DRep {
    head: Head,
    source: Rc<dyn Stream>,
}


impl Describe for DRep {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for DRep {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        DRepIter{iter: self.source.iter(), last: None, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct DRepIter {
    node: Rc<DRep>,
    iter: Box<dyn SIterator>,
    last: Option<Item>
}

impl PreIterator for DRepIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        loop {
            check_stop!();
            let item = iter_try!(self.iter.next());
            if let Some(last) = &self.last && last.try_eq(&item)? {
                continue;
            }
            self.last = Some(item.clone());
            return Ok(Some(item));
        }
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.iter.len_remain())
    }

    fn origin(&self) -> &Rc<DRep> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_drep() {
        use super::*;
        test_eval!("seq.drep" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("(seq/5).drep" => "[0, 1, 2, 3, 4, ...]");
        test_eval!("(1..3):{1..#}.flatten.drep" => "[1, 2, 1, 2, 3]");
        test_eval!("(1..3).repeat(5).drep" => "[1, 2, 3, 1, 2, ...]");
        test_len!("[].drep" => 0);
        test_describe!("seq.drep" => "seq.drep");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("drep", eval_drep, r#"
Keeps only the first of every chain of repeated items.
= stream.?
> [1, 1, 2, 2, 1].? => [1, 2, 1]
: ddup
: reps
"#);
}
