use crate::base::*;

fn eval_alpha(node: &Node, env: &Env) -> Result<Item, StreamError> {
    node.check_no_source()?;
    match &node.args[..] {
        [] => {
            let vec = env.alpha.iter()
                .map(Item::Char)
                .collect::<Vec<_>>();
            Ok(Item::from(vec))
        },
        [alpha, body] => {
            let alpha = match alpha.eval(env)? {
                Item::Stream(stm) => stm.listout()?
                    .into_iter()
                    .map(Item::into_char)
                    .collect::<Result<Vec<_>, _>>()?
                    .try_into()?,
                Item::String(stm) => stm.listout()?.try_into()?,
                _ => return Err(StreamError::usage(&node.head))
            };
            let mut new_env = env.clone();
            new_env.alpha = Rc::new(alpha);
            body.eval(&new_env)
        },
        _ => Err(StreamError::usage(&node.head))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_alpha() {
        use super::*;

        test_eval!("alpha(\"bÁC\"~'ch', 'b' << 'á' << 'c' << 'ch')" => "true");
        test_eval!("alpha(\"báC\", \"b Á c d\"+1)" => "\"á C b d\"");
        test_eval!("alpha(['b', 'á', 'c'], 'B' << 'Á' << 'C')" => "true");
        test_eval!("alpha(['a','b', 1], 0)" => err);
        test_eval!("alpha" => "['a', 'b', 'c', 'd', 'e', ...]");
        test_len!("alpha" => 26);
        test_eval!("alpha(['Á', 'ch'], alpha)" => "['Á', 'ch']");

        test_describe!("alpha(\"cba\", \"abc\".nest{#+1}[3])" => "alpha(['c', 'b', 'a'], ((\"abc\"+1)+1)+1)");
        test_describe!("alpha(\"cba\", \"abc\".nest{#+1})[3]" => "alpha(['c', 'b', 'a'], ((\"abc\"+1)+1)+1)");
        test_describe!("alpha(\"abcd\", \"abc\"+1)" => "alpha(['a', 'b', 'c', 'd'], \"abc\"+1)");
        test_describe!("alpha([], \"abc\"+1)" => "\"abc\"+1");
        test_describe!("alpha(\"abcd\", alpha([], \"abc\"+1))" => "\"abc\"+1");
        test_describe!("alpha(\"abcd\", alpha(\"\", \"abc\"+1))" => "\"abc\"+1");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("alpha", eval_alpha, r#"
Without arguments: returns the current alphabet as a stream of characters.
With arguments: changes alphabet for the evaluation of `expr`.
The new alphabet can be given as a list of characters, or a string.
= ?
= ?(stream, expr)
= ?(string, expr)
> ? => ['a', 'b', 'c', 'd', 'e', ...]
> ?(['α', 'β', 'γ'], ?) => ['α', 'β', 'γ']
> ?(['α', 'β', 'γ'], ["αβα", "γγ", "γαβ", "β"].?sort) => ["αβα", "β", "γαβ", "γγ"]
> ["αβα", "γγ", "γαβ", "β"].?sort => !not in alphabet
> ?("xyz", 'x' + 'x') => 'y'
: ord
: chr
: with
: global
"#);
}
