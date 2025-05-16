use crate::base::*;

fn eval_len(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let RNodeS { source: Item::Stream(ref stm), args: RArgs::Zero, .. } = rnode else {
        return Err(StreamError::new("expected: source.len", rnode));
    };
    match stm.length() {
        Length::Exact(len) => Ok(Item::new_number(len)),
        Length::AtMost(_) | Length::UnknownFinite | Length::Unknown => {
            let mut len = 0;
            for res in stm.iter() {
                check_stop!();
                let _ = res?;
                len.inc();
            }
            Ok(Item::new_number(len))
        },
        Length::Infinite => Err(StreamError::new("stream is infinite", rnode))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_len() {
        use crate::parser::parse;

        assert_eq!(parse("[].len").unwrap().eval_default().unwrap().to_string(), "0");
        assert_eq!(parse("range(10).len").unwrap().eval_default().unwrap().to_string(), "10");
        assert_eq!(parse("range(10).flatten.len").unwrap().eval_default().unwrap().to_string(), "10");
        assert!(parse("1.len").unwrap().eval_default().is_err());
        // Exact len used without checking
        assert_eq!(parse("[1,2,'a']:{1+#}.len").unwrap().eval_default().unwrap().to_string(), "3");
        // Actual enumeration stops at errors
        assert!(parse("[1,2,'a']:{1+#}.flatten.len").unwrap().eval_default().is_err());
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("len", eval_len);
}
