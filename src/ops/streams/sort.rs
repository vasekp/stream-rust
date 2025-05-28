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
        _ => todo!()
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
        use crate::parser::parse;
        assert_eq!(parse("[5,2,9,3,5].sort").unwrap().eval_default().unwrap().to_string(), "[2, 3, 5, 5, 9]");
        //assert!(parse("\"bac\".sort").unwrap().eval_default().is_err());
        assert_eq!(parse("['b','a','c'].sort").unwrap().eval_default().unwrap().to_string(), "['a', 'b', 'c']");
        assert_eq!(parse("[\"abc\", \"def\", \"AdS\"].sort").unwrap().eval_default().unwrap().to_string(), "[\"abc\", \"AdS\", \"def\"]");
        assert_eq!(parse("alpha(\"aáb\", [\"a\", \"B\", \"Á\"].sort)").unwrap().eval_default().unwrap().to_string(), "[\"a\", \"Á\", \"B\"]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("sort", eval_sort);
}
