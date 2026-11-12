use crate::base::*;

fn eval_alpha(node: &Node, env: &Env) -> SResult<Item> {
    node.check_no_source()?;
    match &node.args[..] {
        [] => {
            let vec = env.alpha.iter().map(Item::Char).collect::<Vec<_>>();
            Ok(Item::from(vec))
        },
        [alpha, body] => {
            let alpha = match alpha.eval(env)? {
                Item::Stream(stm) => stm
                    .listout()?
                    .into_iter()
                    .map(Item::into_char)
                    .collect::<SResult<Vec<_>>>()?
                    .try_into()?,
                Item::String(stm) => stm.listout()?.try_into()?,
                _ => return Err(StreamError::usage(&node.head)),
            };
            let mut new_env = env.clone();
            new_env.alpha = Rc::new(alpha);
            body.eval(&new_env)
        },
        _ => Err(StreamError::usage(&node.head)),
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("alpha", eval_alpha, r#"
Without arguments: returns the current alphabet as a stream of characters.
With arguments: changes the alphabet for the evaluation of `expr`.
The new alphabet can be given as a list of characters, or a string. An empty one restores the default.
Letters are matched regardless of case, so each may only appear once. Multi-letter characters and non-letters are allowed.
! The alphabet also defines the ordering for `<<`, `?sort` and character ranges.
* Functions defined by `?with` keep the alphabet in effect where they were defined. Session globals and blocks `${...}` use the default alphabet.
= ?
= ?(stream, expr)
= ?(string, expr)
> ? => ['a', 'b', 'c', 'd', 'e', ...]
> ?(['α', 'β', 'γ'], ?) => ['α', 'β', 'γ']
> ?(['α', 'β', 'γ'], ["αβα", "γγ", "γαβ", "β"].?sort) => ["αβα", "β", "γαβ", "γγ"]
> ["αβα", "γγ", "γαβ", "β"].?sort => !not in alphabet
> ?("xyz", 'x' + 'x') => 'y'
> ?("xyz", 'z' + 1) => 'x'
> ?("xyz", ?([], 'z' + 1)) => 'a'
> ?("xyz", ?with(f={#+1}, ?([], 'z'.f))) => 'x' ; `f` was defined under "xyz"
: ord
: chr
: with
"#);
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
        test_eval!("alpha(\"cba\", \"abc\".nest{#+1})" => "[\"cab\", \"bca\", \"abc\", \"cab\", \"bca\", ...]");
        test_eval!("alpha(\"cba\", ['a', 'c', 'b'].sort)" => "['c', 'b', 'a']");
        test_eval!("alpha(\"cba\", ${['a', 'c', 'b'].sort})" => "['a', 'b', 'c']");

        test_describe!("alpha(\"cba\", \"abc\".nest{#+1}[3])" => "alpha(['c', 'b', 'a'], ((\"abc\"+1)+1)+1)");
        test_describe!("alpha(\"cba\", \"abc\".nest{#+1})[3]" => "alpha(['c', 'b', 'a'], ((\"abc\"+1)+1)+1)");
        test_describe!("alpha(\"abcd\", \"abc\"+1)" => "alpha(['a', 'b', 'c', 'd'], \"abc\"+1)");
        test_describe!("alpha([], \"abc\"+1)" => "\"abc\"+1");
        test_describe!("alpha(\"abcd\", alpha([], \"abc\"+1))" => "\"abc\"+1");
        test_describe!("alpha(\"abcd\", alpha(\"\", \"abc\"+1))" => "\"abc\"+1");
    }

    #[test]
    fn test_alpha_functions() {
        use super::*;
        // An op evaluating a function written in place depends on the alphabet of its construction
        // and describes with an `alpha(...)` wrapper; one holding a closure does not need it.
        test_describe!("alpha(\"cba\", seq.consec{#.isodd})" => "alpha(['c', 'b', 'a'], seq.consec({#.isodd}))");
        test_describe!("alpha(\"cba\", seq.consec(${#.isodd}))" => "seq.consec(${#.isodd})");
        test_describe!("alpha(\"cba\", seq.select{#>2})" => "alpha(['c', 'b', 'a'], seq.select({#>2}))");
        test_describe!("alpha(\"cba\", seq.select(${#>2}))" => "seq.select(${#>2})");
        test_describe!("alpha(\"cba\", seq.while{#<5})" => "alpha(['c', 'b', 'a'], seq.while({#<5}))");
        test_describe!("alpha(\"cba\", seq.while(${#<5}))" => "seq.while(${#<5})");
        test_describe!("alpha(\"cba\", seq.split{#.isodd})" => "alpha(['c', 'b', 'a'], seq.split({#.isodd}))");
        test_describe!("alpha(\"cba\", seq.split(${#.isodd}))" => "seq.split(${#.isodd})");
        test_describe!("alpha(\"cba\", seq.fold{#1+#2})" => "alpha(['c', 'b', 'a'], seq.fold({#1+#2}))");
        test_describe!("alpha(\"cba\", seq.fold(${#1+#2}))" => "seq.fold(${#1+#2})");
        test_describe!("alpha(\"cba\", seq:{#+1})" => "alpha(['c', 'b', 'a'], seq:{#+1})");
        test_describe!("alpha(\"cba\", seq:${#+1})" => "seq:${#+1}");
        test_describe!("alpha(\"cba\", seq:{#+#1}(2))" => "alpha(['c', 'b', 'a'], seq:{#+#1}(2))");
        test_describe!("alpha(\"cba\", \"abc\":{#+1})" => "alpha(['c', 'b', 'a'], \"abc\":{#+1})");
        test_describe!("alpha(\"cba\", \"abc\":${#+1})" => "\"abc\":${#+1}");
        test_describe!("alpha(\"cba\", self{1~(#+1)})" => "alpha(['c', 'b', 'a'], self({1~#+1}))");
        test_describe!("alpha(\"cba\", self(${1~(#+1)}))" => "self(${1~#+1})");
        test_describe!("alpha(\"cba\", seq.groups(2, {#1+#2}))" => "alpha(['c', 'b', 'a'], seq.groups(2, {#1+#2}))");
        test_describe!("alpha(\"cba\", seq.groups(2, ${#1+#2}))" => "seq.groups(2, ${#1+#2})");
        test_describe!("alpha(\"cba\", seq.groups(2))" => "seq.groups(2)");
        test_describe!("alpha(\"cba\", seq.windows(2, {#1+#2}))" => "alpha(['c', 'b', 'a'], seq.windows(2, {#1+#2}))");
        test_describe!("alpha(\"cba\", seq.windows(2, ${#1+#2}))" => "seq.windows(2, ${#1+#2})");
        test_describe!("alpha(\"cba\", seq.windows(2))" => "seq.windows(2)");
        test_describe!("alpha(\"cba\", seq.runs{#/3})" => "alpha(['c', 'b', 'a'], seq.runs({#/3}))");
        test_describe!("alpha(\"cba\", seq.runs(${#/3}))" => "seq.runs(${#/3})");
        test_describe!("alpha(\"cba\", seq.ddup{-#})" => "alpha(['c', 'b', 'a'], seq.ddup({-#}))");
        test_describe!("alpha(\"cba\", seq.ddup(${-#}))" => "seq.ddup(${-#})");
        test_describe!("alpha(\"cba\", seq.ddup)" => "seq.ddup");
        test_describe!("alpha(\"cba\", seq.drep{#%3})" => "alpha(['c', 'b', 'a'], seq.drep({#%3}))");
        test_describe!("alpha(\"cba\", seq.drep(${#%3}))" => "seq.drep(${#%3})");
        test_describe!("alpha(\"cba\", 'a'.nest{#+1})" => "alpha(['c', 'b', 'a'], 'a'.nest({#+1}))");
        test_describe!("alpha(\"cba\", 'a'.nest(${#+1}))" => "'a'.nest(${#+1})");
        test_eval!("alpha(\"cba\", 'a'.nest{#+1})" : 5 => "['c', 'b', 'a', 'c', 'b', ...]");
        test_eval!("alpha(\"cba\", 'a'.nest(${#+1}))" : 5 => "['b', 'c', 'd', 'e', 'f', ...]");
        test_describe!("alpha(\"cba\", nest({#1+#2}(1, 1)))" => "alpha(['c', 'b', 'a'], nest({#1+#2}(1, 1)))");
        test_describe!("alpha(\"cba\", nest(${#1+#2}(1, 1)))" => "nest(${#1+#2}(1, 1))");
        // Ops which also order by the alphabet keep the wrapper even for a closure.
        test_describe!("alpha(\"cba\", seq.rising(${#*2}))" => "alpha(['c', 'b', 'a'], seq.rising(${#*2}))");
        test_describe!("alpha(\"cba\", range(20).sort(${-#}))" => "alpha(['c', 'b', 'a'], range(20).sort(${-#}))");
    }
}
