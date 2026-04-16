use crate::base::*;

use std::cmp::Ordering;

fn eval_sort(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    match &node.args[..] {
        [] => {
            let mut vals = stm.listout()?;
            sort_impl(&mut vals[..], &env.alpha)?;
            Ok(Item::new_stream(List::from(vals)))
        },
        [expr] => {
            let func = expr.as_func()?;
            let mut keyvals = stm.listout_with(|item|
                func.with_source(Expr::from(&item))?
                    .eval(env)
                    .map(|key| (key, item)))?;
            sort_impl(&mut keyvals[..], &env.alpha)?;
            let vals = keyvals.into_iter()
                .map(|(_, val)| val)
                .collect::<Vec<_>>();
            Ok(Item::new_stream(List::from(vals)))
        },
        _ => Err(StreamError::usage(&node.head))
    }
}

trait SortItem {
    fn key(&self) -> &Item;
}

fn sort_impl(vals: &mut [impl SortItem], alpha: &Rc<Alphabet>) -> SResult<()> {
    match &mut vals[..] {
        [] | [_] => (),
        [x, y] => if x.key().lex_cmp(y.key(), alpha)? == Ordering::Greater { std::mem::swap(x, y) },
        _ => {
            let mid = vals.len() / 2;
            vals.swap(0, mid); // TODO unneeded
            let (pivot, rest) = vals.split_first_mut().unwrap(); // checked: len > 2
            let pivot = pivot.key();
            let mut div_ix = 0;
            for ix in 0..rest.len() {
                if rest[ix].key().lex_cmp(pivot, alpha)? == Ordering::Less {
                    rest.swap(ix, div_ix);
                    div_ix += 1;
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

impl SortItem for Item {
    fn key(&self) -> &Item { self }
}

impl SortItem for (Item, Item) {
    fn key(&self) -> &Item { &self.0 }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_sort() {
        use super::*;

        test_eval!("[5,2,9,3,5].sort" => "[2, 3, 5, 5, 9]");
        test_eval!("\"bac\".sort" => err);
        test_eval!("['b','a','c'].sort" => "['a', 'b', 'c']");
        test_eval!("[\"abc\", \"def\", \"AdS\"].sort" => "[\"abc\", \"AdS\", \"def\"]");
        test_eval!("alpha(\"aáb\", [\"a\", \"B\", \"Á\"].sort)" => "[\"a\", \"Á\", \"B\"]");

        test_eval!("[5,2,9,3,5].sort{-#}" => "[9, 5, 5, 3, 2]");
        test_eval!("[\"one\", \"two\", \"three\"].sort(rev)" => "[\"three\", \"one\", \"two\"]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("sort", eval_sort, r#"
Sorts the input `stream`. Arrays of numbers, characters, strings, or streams can be sorted.
Strings and streams are sorted in lexicographic order.
If `func` is provided, compares `item`s using the value of `item.func`.
For pairs of elements `a`, `b` for which `a.func == b.func`, their ordering in the result is unspecified.
= stream.?
= stream.?{func}
> [5, 2, 9, 3, 5].? => [2, 3, 5, 5, 9]
> [5, 2, 9, 3, 5].?{-#} => [9, 5, 5, 3, 2] ; reverse sort
> ['b', 'a', 'c'].? => ['a', 'b', 'c']
> ["one", "two", "three"].? => ["one", "three", "two"]
> ["one", "two", "three", "four"].?(?length) => ["one", "two", "four", "three"]
> [[1,2], [], [1]].? : 6 => [[], [1], [1, 2]]
: <<
: >>
"#);
}
