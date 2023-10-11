use crate::base::*;
use crate::session::Session;
use num::One;


/// A `Stream` formed by direct enumeration of its `Item`s.
pub type List = Vec<Item>;

impl Stream for List {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(self.clone().into_iter().map(|x| Ok(x.clone())))
    }

    fn describe(&self) -> String {
        let mut s = String::new();
        s.push('[');
        let mut it = <[Item]>::iter(self);
        if let Some(item) = it.next() {
            s += &format!("{item}");
            for item in it {
                s += &format!(",{item}");
            }
        }
        s.push(']');
        s
    }

    fn length(&self) -> Length {
        Length::from(self.len())
    }
}

fn construct_list(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(false, 0..)?;
    node.args.iter()
        .map(|x| session.eval(x))
        .collect::<Result<Vec<Item>, _>>()
        .map(Item::new_stream)
}

fn construct_part(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(true, 1..)?;
    let mut item = session.eval(node.source.as_ref().unwrap())?;
    let args = node.args.iter()
        .map(|x| session.eval(x)?.into_num_within(Number::one()..))
        .collect::<Result<Vec<_>, _>>()?;
    for index in args {
        let mut iter = item.into_stream()?.iter();
        if iter.skip_n(&(index - Number::one())).is_err() {
            return Err(BaseError::from("index past end of stream"));
        }
        item = iter.next().unwrap_or(Err(BaseError::from("index past end of stream")))?;
    }
    Ok(item)
}


pub(crate) fn init(session: &mut Session) {
    session.register_symbol("list", construct_list);
    session.register_symbol("part", construct_part);
}
