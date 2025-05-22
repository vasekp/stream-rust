use crate::base::*;

fn eval_alpha(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let rnode = node.resolve_no_source()?;
    let RNodeNS { head, args: RArgs::Two(alpha, body) } = rnode else {
        return Err(StreamError::new("expected: alpha(alphabet, expr)", rnode));
    };
    let errnode = Node { head: head.clone(), source: None, args: vec![] };
    let alpha = alpha.eval(env)?;
    let alpha = try_with!(errnode,
        alpha.to_stream()?
            .listout()?
            .try_into()?);
    let mut new_env = (**env).clone();
    new_env.alpha = Rc::new(alpha);
    body.eval(&Rc::new(new_env))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_alpha() {
        use crate::parser::parse;
        assert_eq!(parse("alpha(\"bÁC\"~'ch', 'b' << 'á' << 'c' << 'ch')").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("alpha(\"báC\", \"b Á c d\".shift(1))").unwrap().eval_default().unwrap().to_string(), "\"á C b d\"");
        assert_eq!(parse("alpha(['b', 'á', 'c'], 'B' << 'Á' << 'C')").unwrap().eval_default().unwrap().to_string(), "true");
        assert!(parse("alpha(['a','b', 1], 0)").unwrap().eval_default().is_err());
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("alpha", eval_alpha);
}
