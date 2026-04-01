use crate::base::*;

fn eval_reps(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let stm = node.source_checked()?.to_stream()?;
    Ok(Item::new_stream(Reps{head: node.head.clone(), source: stm}))
}

struct Reps {
    head: Head,
    source: Rc<dyn Stream>,
}


impl Describe for Reps {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for Reps {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        RepsIter{iter: self.source.iter(), last: None, done: false, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct RepsIter {
    node: Rc<Reps>,
    iter: Box<dyn SIterator>,
    last: Option<Item>,
    done: bool,
}

impl PreIterator for RepsIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.done {
            return Ok(None);
        }
        let curr_item = match self.last.take() {
            Some(last) => last,
            None => iter_try!(self.iter.next()),
        };
        let mut count = 1usize;
        loop {
            check_stop!();
            match self.iter.next()? {
                Some(item) => if !item.try_eq(&curr_item)? {
                    self.last = Some(item);
                    break;
                },
                None => {
                    self.last = None;
                    self.done = true;
                    break;
                }
            }
            count += 1;
        }
        Ok(Some(Item::new_stream(List::from([curr_item, Item::new_number(count)]))))
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.iter.len_remain())
    }

    fn origin(&self) -> &Rc<Reps> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_reps() {
        use super::*;
        test_eval!("seq.reps" : 9 => "[[1, 1], [2, 1], [3, 1], ...]");
        test_eval!("(seq/5).reps" : 9 => "[[0, 4], [1, 5], [2, 5], ...]");
        test_eval!("(1..3):{1..#}.flatten.reps" : 9 => "[[1, 2], [2, 1], [1, 1], ...]");
        test_len!("[].reps" => 0);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["reps", "runs"], eval_reps, r#"
Replaces every chain of repeated items in `stream` by a pair `[item, count]`.
= stream.?
> "mummy".?chars.? : 15 => [['m', 1], ['u', 1], ['m', 2], ['y', 1]]
: drep
: counts
: partitionby
"#);
}
