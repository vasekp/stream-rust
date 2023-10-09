use crate::base::*;
use crate::session::Session;


/// A `Stream` formed by direct enumeration of its `Item`s.
pub type List = Vec<Item>;

impl Stream for List {
    fn iter(&self) -> Box<SIterator> {
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
    assert_eq!(node.source, None);
    node.args.iter().map(|x| session.eval(&x))
        .collect::<Result<Vec<Item>, _>>()
        .map(Item::new_stream)
}


pub(crate) fn init(session: &mut Session) {
    session.register_symbol("list", construct_list);
}
