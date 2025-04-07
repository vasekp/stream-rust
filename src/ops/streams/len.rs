use crate::base::*;

fn eval_len(node: Node, env: &std::rc::Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    if !node.args.is_empty() {
        return Err(StreamError::new("no arguments allowed", node));
    }
    let length = try_with!(node, node.source_checked()?.as_stream()?.length());
    use Length::*;
    match length {
        Exact(len) => Ok(Item::new_number(len)),
        AtMost(_) | UnknownFinite | Unknown => {
            let len = try_with!(node, node.source_checked()?.as_stream()?.iter().count());
            Ok(Item::new_number(len))
        },
        _ => Err(StreamError::new("stream is infinite", node))
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("len", eval_len);
}
