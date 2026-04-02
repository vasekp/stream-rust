use crate::base::*;

use std::collections::VecDeque;

fn eval_startswith(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.as_char_stream()?;
    let mut prefix = node.only_arg_checked()?.to_char_iter()?;
    let mut iter = stm.iter();
    for c1 in prefix.transposed() {
        check_stop!();
        let c1 = c1?;
        if iter.next()?.is_none_or(|c2| c2 != c1) {
            return Ok(Item::Bool(false));
        }
    }
    Ok(Item::Bool(true))
}

fn eval_endswith(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.as_char_stream()?;
    let postfix = node.only_arg_checked()?.to_char_vec()?;
    let pfx_len = postfix.len();
    let mut iter = stm.iter();
    match stm.len() {
        Length::Exact(len) | Length::AtMost(len) if len < UNumber::from(pfx_len)
            => Ok(Item::Bool(false)),
        Length::Exact(len) => {
            iter.advance(&(len - pfx_len))?;
            for (c1, c2) in iter.transposed().zip(postfix.iter()) {
                if c1? != *c2 {
                    return Ok(Item::Bool(false));
                }
            }
            Ok(Item::Bool(true))
        },
        Length::Infinite => Err("stream is infinite".into()),
        _ => {
            let mut vec = VecDeque::with_capacity(pfx_len);
            for res in iter.transposed() {
                check_stop!();
                let item = res?;
                if vec.len() == pfx_len {
                    vec.pop_front();
                }
                vec.push_back(item);
            }
            Ok(Item::Bool(vec == postfix))
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_startswith() {
        use super::*;
        test_eval!("\"abc\".startswith('a')" => "true");
        test_eval!("\"abc\".startswith(\"\")" => "true");
        test_eval!("\"abc\".startswith(\"a\")" => "true");
        test_eval!("\"abc\".startswith(\"ab\")" => "true");
        test_eval!("\"abc\".startswith(\"abcd\")" => "false");
        test_eval!("\"abc\".startswith(\"ac\")" => "false");
        test_eval!("'a'.repeat.startswith(\"aa\")" => "true");
        test_eval!("\"aa\".startswith('a'.repeat)" => "false");
        test_eval!("'a'.startswith('a')" => err);
        test_eval!("['a'].startswith('a')" => err);
    }

    #[test]
    fn test_endswith() {
        use super::*;
        test_eval!("\"abc\".endswith('c')" => "true");
        test_eval!("\"abc\".endswith(\"\")" => "true");
        test_eval!("\"abc\".endswith(\"c\")" => "true");
        test_eval!("\"abc\".endswith(\"bc\")" => "true");
        test_eval!("\"abc\".endswith(\"aabc\")" => "false");
        test_eval!("\"abc\".endswith(\"ac\")" => "false");
        test_eval!("'a'.repeat.endswith(\"aa\")" => err);
        test_eval!("\"aa\".endswith('a'.repeat)" => err);
        test_eval!("'a'.endswith('a')" => err);
        test_eval!("['a'].endswith('a')" => err);
        test_eval!("'a'.repeat(10^10).endswith(\"aa\")" => "true");
        test_eval!("\"ab\".repeat(10).$lenAM.endswith(\"ab\")" => "true");
        test_eval!("\"ab\".repeat(10).$lenUU.endswith(\"ab\")" => "true");
        test_eval!("\"ab\".repeat(10).$lenAM.endswith(\"ba\")" => "false");
        test_eval!("\"ab\".repeat(10).$lenUU.endswith(\"ba\")" => "false");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("startswith", eval_startswith, r#"
Evaluates to `true` if `string` starts with `prefix`, `false` otherwise.
`prefix` can be a string or a character.
= string.?(prefix)
> ["one", "two", "three"].?select{#.?('t')} => ["two", "three"]
: endswith
"#);
    symbols.insert("endswith", eval_endswith, r#"
Evaluates to `true` if `string` ends with `postfix`, `false` otherwise.
`postfix` can be a string or a character.
= string.?(postfix)
> ["one", "two", "three"].?select{#.?('e')} => ["one", "three"]
: startswith
"#);
}
