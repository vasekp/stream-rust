use crate::base::*;

use std::cmp::Ordering;

fn eval_sort(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match &rnode {
        RNodeS { source: Item::Stream(stm), args: RArgs::Zero, .. } => {
            let mut vals = stm.listout()?;
            try_with!(rnode, sort_impl(&mut vals[..], env.alphabet())?);
            Ok(Item::new_stream(List::from(vals)))
        }
        _ => Err(StreamError::new("expected: stream.sort", rnode))
    }
}

fn sort_impl(vals: &mut [Item], alpha: &Rc<Alphabet>) -> Result<(), BaseError> {
    match &mut vals[..] {
        [] | [_] => (),
        [x, y] => if x.lex_cmp(y, alpha)? == Ordering::Greater { std::mem::swap(x, y) },
        _ => {
            let mid = vals.len() / 2;
            vals.swap(0, mid);
            let (pivot, rest) = vals.split_first_mut().unwrap(); // checked: len > 2
            let mut div_ix = 0;
            for ix in 0..rest.len() {
                if rest[ix].lex_cmp(pivot, alpha)? == Ordering::Less {
                    rest.swap(ix, div_ix);
                    div_ix.inc();
                }
            }
            vals.swap(0, div_ix);
            let (s1, s2) = vals.split_at_mut(div_ix + 1); // div+1 > 0 ⇒ both strictly shorter
            sort_impl(s1, alpha)?;
            sort_impl(s2, alpha)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_sort() {
        use super::*;
        use crate::parser::parse;

        test_eval!("[5,2,9,3,5].sort" => "[2, 3, 5, 5, 9]");
        //test_eval!("\"bac\".sort" => err);
        test_eval!("['b','a','c'].sort" => "['a', 'b', 'c']");
        test_eval!("[\"abc\", \"def\", \"AdS\"].sort" => "[\"abc\", \"AdS\", \"def\"]");
        test_eval!("alpha(\"aáb\", [\"a\", \"B\", \"Á\"].sort)" => "[\"a\", \"Á\", \"B\"]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("sort", eval_sort);
}
