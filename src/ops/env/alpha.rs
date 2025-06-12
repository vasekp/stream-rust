use crate::base::*;

fn eval_alpha(node: Node, env: &Env) -> Result<Item, StreamError> {
    if node.source.is_none() && node.args.is_empty() {
        let vec = env.alpha.iter()
            .map(|ch| Item::Char(Char::from(ch)))
            .collect::<Vec<_>>();
        return Ok(Item::from(vec));
    }
    let rnode = node.eval_nth_arg(0, env)?.resolve_no_source()?;
    let RNodeNS { args: RArgs::Two(alpha, _), .. } = &rnode else {
        return Err(StreamError::new("expected: alpha(alphabet, expr)", rnode));
    };
    let alpha = try_with!(rnode, {
        match alpha {
            Expr::Imm(Item::Stream(stm)) => stm.listout()?
                .into_iter()
                .map(Item::into_char)
                .collect::<Result<Vec<_>, _>>()?
                .try_into()?,
            Expr::Imm(Item::String(stm)) => stm.listout()?.try_into()?,
            _ => return Err(BaseError::from(format!("expected stream or string, found {:?}", alpha)))
        }
    });
    let mut new_env = env.clone();
    new_env.alpha = Rc::new(alpha);
    let RArgs::Two(_, body) = rnode.args else { unreachable!() };
    body.eval(&new_env)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_alpha() {
        use super::*;
        use crate::parser::parse;
        test_eval!("alpha(\"bÁC\"~'ch', 'b' << 'á' << 'c' << 'ch')" => "true");
        test_eval!("alpha(\"báC\", \"b Á c d\"+1)" => "\"á C b d\"");
        test_eval!("alpha(['b', 'á', 'c'], 'B' << 'Á' << 'C')" => "true");
        test_eval!("alpha(['a','b', 1], 0)" => err);
        test_eval!("alpha" => "['a', 'b', 'c', 'd', 'e', ...]");
        test_len!("alpha" => 26);
        test_eval!("alpha(['Á', 'ch'], alpha)" => "['Á', 'ch']");

        test_describe!("alpha(\"cba\", \"abc\".nest{#+1}[3])" => "alpha(['c', 'b', 'a'], ((\"abc\"+1)+1)+1)");
        test_describe!("alpha(\"cba\", \"abc\".nest{#+1})[3]" => "alpha(['c', 'b', 'a'], ((\"abc\"+1)+1)+1)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("alpha", eval_alpha);
}
