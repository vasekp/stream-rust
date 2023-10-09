use crate::base::*;


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
