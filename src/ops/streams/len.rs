use crate::base::*;

fn eval_len(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let RNodeS { source: Item::Stream(ref stm), args: RArgs::Zero, .. } = rnode else {
        return Err(StreamError::new("expected: source.len", rnode));
    };
    match stm.length() {
        Length::Exact(len) => Ok(Item::new_number(len)),
        Length::AtMost(_) | Length::UnknownFinite | Length::Unknown => {
            let len = stm.iter().count();
            Ok(Item::new_number(len))
        },
        Length::Infinite => Err(StreamError::new("stream is infinite", rnode))
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("len", eval_len);
}
