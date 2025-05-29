use crate::base::*;

fn eval_alpha(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_nth_arg(0, env)?.resolve_no_source()?;
    let RNodeNS { args: RArgs::Two(alpha, _), .. } = &rnode else {
        return Err(StreamError::new("expected: alpha(alphabet, expr)", rnode));
    };
    let alpha = try_with!(rnode, {
        let Expr::Imm(Item::Stream(stm) | Item::String(stm)) = &alpha else {
            return Err(BaseError::from(format!("expected stream or string, found {:?}", alpha)));
        };
        stm.listout()?.try_into()?
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
        assert_eq!(parse("alpha(\"bÁC\"~'ch', 'b' << 'á' << 'c' << 'ch')").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("alpha(\"báC\", \"b Á c d\"+1)").unwrap().eval_default().unwrap().to_string(), "\"á C b d\"");
        assert_eq!(parse("alpha(['b', 'á', 'c'], 'B' << 'Á' << 'C')").unwrap().eval_default().unwrap().to_string(), "true");
        assert!(parse("alpha(['a','b', 1], 0)").unwrap().eval_default().is_err());

        assert_eq!(parse("alpha(\"cba\", \"abc\".nest{#+1}[3])").unwrap().eval_default().unwrap().describe(), "alpha(['c', 'b', 'a'], ((\"abc\"+1)+1)+1)");
        assert_eq!(parse("alpha(\"cba\", \"abc\".nest{#+1})[3]").unwrap().eval_default().unwrap().describe(), "alpha(['c', 'b', 'a'], ((\"abc\"+1)+1)+1)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("alpha", eval_alpha);
}
