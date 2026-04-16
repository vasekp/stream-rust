use crate::base::*;

fn eval_ddup(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = match &node.args[..] {
        [] => None,
        [expr] => Some((Rc::clone(expr.as_func()?), env.clone())),
        _ => return Err(StreamError::usage(&node.head)),
    };
    Ok(Item::new_stream(DDup{head: node.head.clone(), source: stm, func}))
}

struct DDup {
    head: Head,
    source: Rc<dyn Stream>,
    func: Option<(Rc<Node>, Env)>,
}

impl Describe for DDup {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(self.func.as_ref().map(|(func, _)| &**func))
            .finish(prec)
    }
}

impl Stream for DDup {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        DDupIter{iter: self.source.iter(), seen: vec![], node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct DDupIter {
    node: Rc<DDup>,
    iter: Box<dyn SIterator>,
    seen: Vec<Item>
}

impl PreIterator for DDupIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        'a: loop {
            check_stop!();
            let item = iter_try!(self.iter.next());
            let key = match &self.node.func {
                None => item.clone(),
                Some((func, env)) => func.with_source(Expr::from(&item))?.eval(env)?,
            };
            for seen in &self.seen {
                if key.try_eq(seen)? {
                    continue 'a;
                }
            }
            self.seen.push(key);
            return Ok(Some(item));
        }
    }

    fn origin(&self) -> &Rc<DDup> {
        &self.node
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
        test_len!("[].ddup" => 0);
        test_describe!("seq.ddup" => "seq.ddup");

        test_eval!("seq.ddup{#/5}" => "[1, 5, 10, 15, 20, ...]");
        test_eval!("[1, 2, 4, 5, 6].ddup{#%3}" => "[1, 2, 6]");
        test_eval!("(1..3).repeat(5).ddup{#}" => "[1, 2, 3]");
        test_len!("[].ddup{#+'a'}" => 0);
        test_describe!("seq.ddup{#}" => "seq.ddup({#})");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["ddup", "uniq"], eval_ddup, r#"
Keeps only the first appearance of any repeated item in `stream`.
If `func` is provided, compares `item.func` for the uniqueness instead of `item` itself.
= stream.?
= stream.?(func)
> [1, 2, 1, 2, 3].? => [1, 2, 3]
> [1, 2, 4, 5, 6].?{#%3} => [1, 2, 6]
> "Abracadabra".?chars.? : 6 => ['A', 'b', 'r', 'a', 'c', 'd']
> "Abracadabra".?chars.?(?lcase) => ['A', 'b', 'r', 'c', 'd']
: drep
: counts
"#);
}
