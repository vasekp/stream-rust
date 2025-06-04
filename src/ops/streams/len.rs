use crate::base::*;

fn eval_len(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let len = match &rnode {
        RNodeS { source: Item::Stream(stm), args: RArgs::Zero, .. } => len_impl(&**stm),
        RNodeS { source: Item::String(stm), args: RArgs::Zero, .. } => len_impl(&**stm),
        _ => return Err(StreamError::new("expected: source.len", rnode))
    };
    Ok(Item::new_number(try_with!(rnode, len?)))
}

fn len_impl<ItemType>(stm: &dyn Stream<ItemType>) -> Result<UNumber, BaseError> {
    match stm.length() {
        Length::Exact(len) => Ok(len),
        Length::AtMost(_) | Length::UnknownFinite | Length::Unknown => {
            let mut len = 0usize;
            for res in stm.iter() {
                check_stop!();
                let _ = res?;
                len.inc();
            }
            Ok(len.into())
        },
        Length::Infinite => Err("stream is infinite".into())
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
