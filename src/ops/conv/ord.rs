use crate::base::*;

fn eval_ord(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_args()?);
    let Some(Item::Char(ch)) = &node.source else {
        return Err(StreamError::new("expected: character.ord", node));
    };
    let ix = try_with!(node, env.alphabet().ord(ch)?);
    Ok(Item::new_number(ix))
}

fn eval_chr(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_args()?);
    let Some(Item::Number(ix)) = &node.source else {
        return Err(StreamError::new("expected: number.chr", node));
    };
    let ch = env.alphabet().chr(ix, CharCase::Indeterminate);
    Ok(Item::new_char(ch))
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

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("ord", eval_ord);
    keywords.insert("chr", eval_chr);
}
