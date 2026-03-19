use crate::base::*;

fn eval_ord(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let ch = node.source_checked()?.as_char()?;
    let ix = match &node.args[..] {
        [] => env.alpha.ord(ch)?,
        [Item::Stream(stm)] => to_alpha(stm)?.ord(ch)?,
        [Item::String(stm)] => Alphabet::try_from(stm.listout()?)?.ord(ch)?,
        _ => return Err(StreamError::usage(&node.head)),
    };
    Ok(Item::new_number(ix))
}

fn eval_chr(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let ix = node.source_checked()?.as_num()?;
    let ch = match &node.args[..] {
        [] => env.alpha.chr(ix, CharCase::Indeterminate),
        [Item::Stream(stm)] => to_alpha(stm)?.chr(ix, CharCase::Indeterminate),
        [Item::String(stm)] => Alphabet::try_from(stm.listout()?)?
            .chr(ix, CharCase::Indeterminate),
        _ => return Err(StreamError::usage(&node.head)),
    };
    Ok(Item::new_char(ch))
}

fn to_alpha(stm: &Rc<dyn Stream>) -> SResult<Alphabet> {
    stm.listout()?
        .into_iter()
        .map(Item::into_char)
        .collect::<SResult<Vec<_>>>()?
        .try_into()
}

fn eval_uniord(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let ch = node.source_checked()?.as_char()?;
    match ch {
        Char::Single(c) => Ok(Item::new_number(*c as u32)),
        Char::Multi(_) => Err("not in Unicode".into())
    }
}

fn eval_unichr(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let ix = node.source_checked()?.as_num()?.try_cast()?;
    match char::from_u32(ix) {
        Some(ch) => Ok(Item::new_char(ch)),
        None => Err("invalid Unicode codepoint".into())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ord() {
        use super::*;
        test_eval!("'z'.ord" => "26");
        test_eval!("\"abc\".ord" => err);
        test_eval!("\"abc\".chars:ord" => "[1, 2, 3]");
        test_eval!("\"x y z\".chars.select(isalpha):ord" => "[24, 25, 26]");
        test_eval!("alpha(\"bad\",\"abc\".chars:ord)" => "[2, 1, <!>");
    }

    #[test]
    fn test_chr() {
        use super::*;
        test_eval!("((-2)..2):chr" => "['x', 'y', 'z', 'a', 'b']");
        test_eval!("\"abc\".chars:ord:{#+13}:chr.string" => "\"nop\"");
        test_eval!("alpha(['á', 'ch'],100.chr)" => "'ch'");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("ord", eval_ord, r#"
Converts `char` to its order in the current alphabet, or one provided in the argument
(in the same form as for `?alpha`).
= char.?
= char.?(string or stream)
> 'z'.? => 26
> ?alpha("αβγ", 'Γ'.?) => 3
> "Hello".?chars:? => [8, 5, 12, 12, 15]
> "typewriter".?chars:?("qwertyuiop") : 10 => [5, 6, 10, 3, 2, 4, 8, 5, 3, 4]
: alpha
: chr
: uniord
"#);
    symbols.insert("chr", eval_chr, r#"
Converts `number` to character in the current alphabet (lowercase).
If a custom alphabet is provided in the argument (in same form as for `?alpha`), it is used instead.
* This function wraps automatically.
= number.?
= number.?(string or stream)
> 26.? => 'z'
> [8, 5, 12, 12, 15]:?.?string => "hello"
> ?range(25, 28):? => ['y', 'z', 'a', 'b'] ; wraps automatically
> ?alpha("αβγ", 2.?) => 'β'
> [3, 2, 1]:chr("qwerty") => ['e', 'w', 'q']
: alpha
: ord
: unichr
"#);
    symbols.insert("uniord", eval_uniord, r#"
Converts `char` to its Unicode code.
= char.?
> 'z'.? => 122
> 'α'.? => 945
> "Hello".?chars:? => [72, 101, 108, 108, 111]
: unichr
: ord
"#);
    symbols.insert("unichr", eval_unichr, r#"
Converts `number` to a character at its Unicode codepoint.
= number.?
> 0x2192.unichr => '→'
> (0x1F311..0x1F318):unichr : 10 => ['🌑', '🌒', '🌓', '🌔', '🌕', '🌖', '🌗', '🌘']
> 0xD900.unichr => !invalid Unicode ; A surrogate codepoint
: uniord
: chr
"#);
}
