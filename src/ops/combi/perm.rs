use crate::base::*;

fn eval_perm(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?.resolve_source()?;
    match node {
        RNodeS { source: Item::Stream(ref stm), args: RArgs::Zero, head } => {
            if stm.len() == Length::Infinite {
                Ok(Item::new_stream(PermStream { source: node.source, len: None, head }))
            } else {
                let count = stm.try_count()?;
                Ok(Item::new_stream(PermStream { source: node.source, len: Some(count), head }))
            }
        },
        _ => Err(StreamError::new("expected: stream.perm", node))
    }
}

#[derive(Clone)]
struct PermStream {
    source: Item,
    len: Option<UNumber>,
    head: Head,
}

impl Describe for PermStream {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for PermStream {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(PermIter {
            source: &self.source,
            src_len: &self.len,
            self_len: self.len.as_ref().and_then(|x| x.to_u32().map(crate::utils::factorial)),
            order: vec![],
            num_read: UNumber::zero()
        })
    }

    fn len(&self) -> Length {
        match &self.len {
            Some(len) => match len.to_u32() {
                Some(x) => Length::Exact(crate::utils::factorial(x)),
                None => Length::UnknownFinite
            },
            None => Length::Infinite
        }
    }
}

fn build_order(mut n: UNumber) -> Vec<usize> {
    if n.is_zero() { return vec![]; } else { n.dec(); }
    let mut vec = vec![0];
    for i in 2usize.. {
        if n.is_zero() { break; }
        let (div, rem) = n.div_rem(&UNumber::from(i));
        vec.push(rem.to_usize().unwrap()); // result of div must fit
        n = div;
    }
    vec.reverse();
    let mut elms = (0..vec.len()).rev().collect::<Vec<_>>();
    for elm in &mut vec {
        *elm = elms.remove(*elm);
    }
    vec.reverse();
    vec
}

struct PermIter<'node> {
    source: &'node Item,
    src_len: &'node Option<UNumber>,
    self_len: Option<UNumber>,
    order: Vec<usize>,
    num_read: UNumber,
}

impl Iterator for PermIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.order.is_empty() {
            self.order = vec![0];
        } else {
            match self.order.windows(2).position(|s| s[0] < s[1]) {
                Some(ix) => {
                    let y = self.order[ix + 1];
                    let ix2 = self.order.iter().position(|x| x < &y)
                        .unwrap(); // at least the directly preceding element is <
                    self.order.swap(ix + 1, ix2);
                    self.order[0..=ix].sort();
                },
                None => {
                    let prev_len = self.order.len();
                    if self.src_len.as_ref()
                        .and_then(UNumber::to_usize)
                        .is_some_and(|x| x == prev_len) { return None; }
                    self.order.push(self.order[0]);
                    self.order[0] = prev_len;
                    self.order[0..prev_len].sort();
                }
            }
        }
        let order = self.order.iter()
            .map(|x| Item::new_number(x + 1))
            .collect::<Vec<_>>();
        self.num_read.inc();
        Some(Expr::from(ENode {
            head: "reorder".into(),
            source: Some(self.source.clone()),
            args: order
        }).eval_default())
    }
}

impl SIterator for PermIter<'_> {
    fn len_remain(&self) -> Length {
        match (self.src_len, &self.self_len) {
            (None, _) => Length::Infinite,
            (Some(_), Some(ref len)) => Length::Exact(len - &self.num_read),
            (Some(_), None) => Length::UnknownFinite
        }
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.num_read += n;
        if let Some(len) = &self.self_len {
            if &self.num_read >= len {
                return Ok(Some(&self.num_read - len));
            }
        }
        self.order = build_order(self.num_read.clone());
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_perm() {
        use super::*;
        test_eval!("range(10).perm.len" => "3628800");
        test_eval!("range(100).perm.len.numstr.len" => "158");
        test_eval!("seq.perm.len" => err);
        test_eval!("[2,4,6].perm" : 24 => "[[2, 4, 6], [4, 2, 6], [2, 6, 4], [6, 2, 4], [4, 6, 2], [6, 4, 2]]");
        test_eval!("[2,4,6].skip(1).perm" : 10 => "[[4, 6], [6, 4]]");
        test_eval!("alpha.perm.rnd(0).first.string" => "\"brpdlaqkyvgcnztwxeuf...");
        test_len!("[2,4,6].perm" => 6);
        test_len!("range(10^50).perm" => Length::UnknownFinite);
        test_len!("seq.perm" => Length::Infinite);
        test_advance("[2,4,6].perm");
        test_advance("range(10).perm");
        test_advance("seq.perm:first(5)");
        test_describe!("[2,4,6].perm" => "[2, 4, 6].perm");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("perm", eval_perm);
}
