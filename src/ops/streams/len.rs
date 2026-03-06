use crate::base::*;

fn eval_len(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let len = match node.source_checked()? {
        Item::Stream(stm) => len_impl(&**stm)?,
        Item::String(stm) => len_impl(&**stm)?,
        _ => return Err(StreamError::usage(&node.head))
    };
    Ok(Item::new_number(len))
}

fn len_impl<I>(stm: &dyn Stream<I>) -> Result<UNumber, StreamError> {
    match stm.len() {
        Length::Exact(len) => Ok(len),
        Length::AtMost(_) | Length::UnknownFinite | Length::Unknown => {
            let mut len = 0usize;
            for res in stm.iter().transposed() {
                check_stop!();
                let _ = res?;
                len += 1;
            }
            Ok(len.into())
        },
        Length::Infinite => Err(StreamError::new0("stream is infinite"))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_len() {
        use super::*;

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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["len", "length"], eval_len, r#"
Counts the number of items in `stream`, or the number of characters in `string`.
= stream.?
= string.?
> [2, 4, 6].? => 3
> "abcde".? => 5
> ?seq.? => !stream is infinite
"#);
}
