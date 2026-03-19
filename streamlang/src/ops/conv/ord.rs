use crate::base::*;

fn eval_ord(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let ch = node.source_checked()?.as_char()?;
    let ix = env.alpha.ord(ch)?;
    Ok(Item::new_number(ix))
}

fn eval_chr(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let ix = node.source_checked()?.as_num()?;
    let ch = env.alpha.chr(ix, CharCase::Indeterminate);
    Ok(Item::new_char(ch))
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
Converts `char` to its order in the current alphabet.
= char.?
> 'z'.? => 26
> ?alpha("αβγ", 'Γ'.?) => 3
> "Hello".?chars:? => [8, 5, 12, 12, 15]
: alpha
: chr
: uniord
"#);
    symbols.insert("chr", eval_chr, r#"
Converts `number` to character in alphabet (lowercase).
* This function wraps automatically.
= number.?
> 26.? => 'z'
> [8, 5, 12, 12, 15]:?.?string => "hello"
> ?range(25, 28):? => ['y', 'z', 'a', 'b'] ; wraps automatically
> ?alpha("αβγ", 2.?) => 'β'
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
