use crate::base::*;

#[derive(Clone)]
struct LenAM {
    head: Head,
    src: BoxedStream,
}

impl LenAM {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        match rnode {
            RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero }
                => Ok(Item::new_stream(LenAM { head, src: stm.into() })),
            RNodeS { head, source: Item::String(stm), args: RArgs::Zero }
                => Ok(Item::new_string_stream(LenAM { head, src: stm.into() })),
            _ => panic!()
        }
    }
}

impl Describe for LenAM {
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, Some(&self.src), None::<&Item>, prec)
    }
}

impl Stream for LenAM {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(LenAMIter { iter: self.src.iter() })
    }

    fn length(&self) -> Length {
        Length::at_most(self.src.length())
    }
}

struct LenAMIter<'node> {
    iter: Box<dyn SIterator + 'node>
}

impl Iterator for LenAMIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl SIterator for LenAMIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.skip_n(n)
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.iter.len_remain())
    }
}

#[derive(Clone)]
struct LenUF {
    head: Head,
    src: BoxedStream,
}

impl LenUF {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        match rnode {
            RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero }
                => Ok(Item::new_stream(LenUF { head, src: stm.into() })),
            RNodeS { head, source: Item::String(stm), args: RArgs::Zero }
                => Ok(Item::new_string_stream(LenUF { head, src: stm.into() })),
            _ => panic!()
        }
    }
}

impl Describe for LenUF {
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, Some(&self.src), None::<&Item>, prec)
    }
}

impl Stream for LenUF {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(LenUFIter { iter: self.src.iter() })
    }

    fn length(&self) -> Length {
        Length::UnknownFinite
    }
}

struct LenUFIter<'node> {
    iter: Box<dyn SIterator + 'node>
}

impl Iterator for LenUFIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl SIterator for LenUFIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.skip_n(n)
    }

    fn len_remain(&self) -> Length {
        Length::UnknownFinite
    }
}

#[derive(Clone)]
struct LenUU {
    head: Head,
    src: BoxedStream,
}

impl LenUU {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        match rnode {
            RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero }
                => Ok(Item::new_stream(LenUU { head, src: stm.into() })),
            RNodeS { head, source: Item::String(stm), args: RArgs::Zero }
                => Ok(Item::new_string_stream(LenUU { head, src: stm.into() })),
            _ => panic!()
        }
    }
}

impl Describe for LenUU {
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, Some(&self.src), None::<&Item>, prec)
    }
}

impl Stream for LenUU {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(LenUUIter { iter: self.src.iter() })
    }

    fn length(&self) -> Length {
        Length::Unknown
    }
}

struct LenUUIter<'node> {
    iter: Box<dyn SIterator + 'node>
}

impl Iterator for LenUUIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl SIterator for LenUUIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.skip_n(n)
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("lenAM", LenAM::eval);
    keywords.insert("lenUF", LenUF::eval);
    keywords.insert("lenUU", LenUU::eval);
}
