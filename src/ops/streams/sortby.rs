use crate::base::*;

use std::cmp::Ordering;

fn eval_sortby(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_source(env)?;
    match &rnode {
        RNodeS { source: Item::Stream(stm), args: RArgs::One(Expr::Eval(func)), .. } => {
            let mut vals_keys = stm.listout()?
                .into_iter()
                .map(|item| -> Result<(Item, Item), StreamError> {
                    func.clone()
                        .with_source(item.clone().into())?
                        .eval(env)
                        .map(|res| (item, res))
                })
                .collect::<Result<Vec<_>, _>>()?;
            try_with!(rnode, sortby_impl(&mut vals_keys[..], env.alphabet())?);
            let vals = vals_keys.into_iter()
                .map(|(val, _)| val)
                .collect::<Vec<_>>();
            Ok(Item::new_stream(List::from(vals)))
        }
        _ => Err(StreamError::new("expected: stream.sortby{function}", rnode))
    }
}

fn sortby_impl(vals: &mut [(Item, Item)], alpha: &Rc<Alphabet>) -> Result<(), BaseError> {
    match &mut vals[..] {
        [] | [_] => (),
        [x, y] => if x.1.lex_cmp(&y.1, alpha)? == Ordering::Greater { std::mem::swap(x, y) },
        _ => {
            let mid = vals.len() / 2;
            vals.swap(0, mid);
            let (pivot, rest) = vals.split_first_mut().unwrap(); // checked: len > 2
            let pivot = &pivot.1;
            let mut div_ix = 0;
            for ix in 0..rest.len() {
                if rest[ix].1.lex_cmp(pivot, alpha)? == Ordering::Less {
                    rest.swap(ix, div_ix);
                    div_ix.inc();
                }
            }
            vals.swap(0, div_ix);
            let (s1, s2) = vals.split_at_mut(div_ix + 1); // div+1 > 0 ⇒ both strictly shorter
            sortby_impl(s1, alpha)?;
            sortby_impl(s2, alpha)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_sortby() {
        use super::*;
        use crate::parser::parse;

        test_eval!("[5,2,9,3,5].sortby{-#}" => "[9, 5, 5, 3, 2]");
        test_eval!("[\"one\", \"two\", \"three\"].sortby(rev)" => "[\"three\", \"one\", \"two\"]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("sortby", eval_sortby);
}
