use crate::base::*;
use super::util::{factorial, factorial_capped};

/// Source lengths up to this value have their permutation count computed up front and reported
/// as an exact length; longer sources report `UnknownFinite`.
const PERM_EXACT_LEN_MAX: u32 = 10_000;

fn eval_perm(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let stm = node.source_checked()?.to_stream()?;
    if stm.len() == Length::Infinite {
        Ok(Item::new_stream(PermStream { source: stm, len: None, count: None, head: node.head.clone() }))
    } else {
        let len = stm.try_count()?;
        let count = match u32::try_from(&len) {
            Ok(x) if x <= PERM_EXACT_LEN_MAX => Some(factorial(x)?),
            _ => None,
        };
        Ok(Item::new_stream(PermStream { source: stm, len: Some(len), count, head: node.head.clone() }))
    }
}

struct PermStream {
    source: Rc<dyn Stream>,
    /// Length of the source, `None` if infinite.
    len: Option<UNumber>,
    /// Number of permutations (`len!`), if the source is short enough for it to be computed.
    count: Option<UNumber>,
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
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        PermIter { order: vec![], num_read: UNumber::zero(), node: self }.wrap()
    }

    fn len(&self) -> Length {
        match (&self.len, &self.count) {
            (None, _) => Length::Infinite,
            (Some(_), Some(count)) => Length::Exact(count.clone()),
            (Some(_), None) => Length::UnknownFinite,
        }
    }
}

fn build_order(mut n: UNumber) -> Vec<usize> {
    if n.is_zero() {
        return vec![];
    } else {
        n -= 1;
    }
    let mut vec = vec![0];
    for i in 2usize.. {
        if n.is_zero() {
            break;
        }
        let (div, rem) = n.div_rem(&UNumber::from(i));
        vec.push(usize::try_from(rem).unwrap()); // result of div must fit
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

struct PermIter {
    node: Rc<PermStream>,
    order: Vec<usize>,
    num_read: UNumber,
}

impl PreIterator for PermIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.order.is_empty() {
            self.order = vec![0];
        } else {
            match self.order.windows(2).position(|s| s[0] < s[1]) {
                Some(ix) => {
                    let y = self.order[ix + 1];
                    let ix2 = self.order.iter().position(|x| x < &y).unwrap(); // at least the directly preceding element is <
                    self.order.swap(ix + 1, ix2);
                    self.order[0..=ix].sort();
                },
                None => {
                    let prev_len = self.order.len();
                    if self
                        .node
                        .len
                        .as_ref()
                        .and_then(|x| usize::try_from(x).ok())
                        .is_some_and(|x| x == prev_len)
                    {
                        return Ok(None);
                    }
                    self.order.push(self.order[0]);
                    self.order[0] = prev_len;
                    self.order[0..prev_len].sort();
                },
            }
        }
        let order = self.order.iter().map(|x| Item::new_number(x + 1)).collect::<Vec<_>>();
        self.num_read += 1;
        Expr::from(Node { head: "reorder".into(), source: Some(Item::from(&self.node.source)), args: order })
            .eval_default()
            .map(Option::Some)
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.num_read += n;
        // The stream ends after len! permutations. If that number was not precomputed, the
        // factorial is only computed as far as needed to decide whether num_read exceeds it.
        let count = match (&self.node.count, &self.node.len) {
            (Some(count), _) => (&self.num_read >= count).then(|| count.clone()),
            (None, Some(len)) => match u32::try_from(len) {
                Ok(len) => factorial_capped(len, &self.num_read)?,
                Err(_) => None,
            },
            (None, None) => None,
        };
        if let Some(count) = count {
            return Ok(Some(&self.num_read - count));
        }
        self.order = build_order(self.num_read.clone());
        Ok(None)
    }

    fn origin(&self) -> &Rc<PermStream> {
        &self.node
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("perm", eval_perm, r#"
A stream of all permutations of the input `stream`, listed in reverse lexicographical order.
= stream.?
> [2, 4, 6].? : 16 => [[2, 4, 6], [4, 2, 6], [2, 6, 4], [6, 2, 4], ...]
> [1, 2, 3, 4].?.?length => 24
> ?seq.?[100] : 10 => [4, 2, 3, 5, 1, 6, 7, 8, 9, 10, ...] ; input can be infinite
: factorial
: reorder
"#);
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
        test_eval!("alpha.perm.rnd(0).first.string" => "\"uorclhnmzvjdsfqiwtbp...");
        test_len!("[2,4,6].perm" => 6);
        test_eval!("range(10^4).perm.len.numstr.len" => "35660");
        test_len!("range(10^4+1).perm" => Length::UnknownFinite);
        // No factorial of the source length is computed unless the end is actually approached
        test_eval!("range(10^7).perm.first.first" => "1");
        test_eval!("range(10^7).perm[2].first" => "2");
        test_eval!("range(10^7).perm[3].first" => "1");
        test_eval!("range(10^7).perm.skip(10^30).first.first(5) == range(10^7).perm[10^30+1].first(5)" => "true");
        test_eval!("range(5).perm[120]" => "[5, 4, 3, 2, 1]");
        test_eval!("range(5).perm[121]" => err);
        test_eval!("range(5).perm.skip(119)" : 10 => "[[5, 4, 3, 2, 1]]");
        test_eval!("[2,4,6].perm.skip(6)" => "[]");
        test_eval!("[2,4,6].perm.skip(5)" : 10 => "[[6, 4, 2]]");
        test_eval!("([2,4,6].perm~[0]).skip(6)" : 10 => "[0]");
        test_eval!("([2,4,6].perm~[0]).skip(7)" : 10 => "[]");
        test_advance("[2,4,6].perm");
        test_advance("range(4).perm");
        test_len!("range(10^50).perm" => Length::UnknownFinite);
        test_len!("seq.perm" => Length::Infinite);
        test_advance("[2,4,6].perm");
        test_advance("range(10).perm");
        test_advance("seq.perm:first(5)");
        test_describe!("[2,4,6].perm" => "[2, 4, 6].perm");
    }
}
