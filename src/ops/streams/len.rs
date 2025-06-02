use crate::base::*;

fn eval_len(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let RNodeS { source: Item::Stream(ref stm) | Item::String(ref stm), args: RArgs::Zero, .. } = rnode else {
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
        use super::*;
        use crate::parser::parse;

        test_eval!("[].len" => "0");
        test_eval!("range(10).len" => "10");
        test_eval!("range(10).flatten.len" => "10");
        test_eval!("\"abc\".len" => "3");
        test_eval!("1.len" => err);
        // Exact len used without checking
        test_eval!("[1,2,'a']:{1+#}.len" => "3");
        // Actual enumeration stops at errors
        test_eval!("[1,2,'a']:{1+#}.flatten.len" => err);
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("len", eval_len);
}
