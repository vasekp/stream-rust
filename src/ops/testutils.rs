use crate::base::*;

fn eval_len_am(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match rnode {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero }
            => Ok(Item::new_stream(LenAM { head, src: stm.into() })),
        RNodeS { head, source: Item::String(stm), args: RArgs::Zero }
            => Ok(Item::new_string(LenAM { head, src: stm.into() })),
        _ => panic!()
    }
}

#[derive(Clone)]
struct LenAM<I: ItemType> {
    head: Head,
    src: BoxedStream<I>,
}

impl<I: ItemType> Describe for LenAM<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.src), None::<&Item>, prec, env)
    }
}

impl<I: ItemType> Stream<I> for LenAM<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        Box::new(LenAMIter { iter: self.src.iter() })
    }

    fn len(&self) -> Length {
        Length::at_most(self.src.len())
    }
}

struct LenAMIter<'node, I: ItemType> {
    iter: Box<dyn SIterator<I> + 'node>
}

impl<I: ItemType> Iterator for LenAMIter<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<I: ItemType> SIterator<I> for LenAMIter<'_, I> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.iter.len_remain())
    }
}

fn eval_len_uf(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match rnode {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero }
            => Ok(Item::new_stream(LenUF { head, src: stm.into() })),
        RNodeS { head, source: Item::String(stm), args: RArgs::Zero }
            => Ok(Item::new_string(LenUF { head, src: stm.into() })),
        _ => panic!()
    }
}

#[derive(Clone)]
struct LenUF<I: ItemType> {
    head: Head,
    src: BoxedStream<I>,
}

impl<I: ItemType> Describe for LenUF<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.src), None::<&Item>, prec, env)
    }
}

impl<I: ItemType> Stream<I> for LenUF<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        Box::new(LenUFIter { iter: self.src.iter() })
    }

    fn len(&self) -> Length {
        Length::UnknownFinite
    }
}

struct LenUFIter<'node, I: ItemType> {
    iter: Box<dyn SIterator<I> + 'node>
}

impl<I: ItemType> Iterator for LenUFIter<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<I: ItemType> SIterator<I> for LenUFIter<'_, I> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        Length::UnknownFinite
    }
}

fn eval_len_uu(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match rnode {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero }
            => Ok(Item::new_stream(LenUU { head, src: stm.into() })),
        RNodeS { head, source: Item::String(stm), args: RArgs::Zero }
            => Ok(Item::new_string(LenUU { head, src: stm.into() })),
        _ => panic!()
    }
}

#[derive(Clone)]
struct LenUU<I: ItemType> {
    head: Head,
    src: BoxedStream<I>,
}

impl<I: ItemType> Describe for LenUU<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.src), None::<&Item>, prec, env)
    }
}

impl<I: ItemType> Stream<I> for LenUU<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        Box::new(LenUUIter { iter: self.src.iter() })
    }

    fn len(&self) -> Length {
        Length::Unknown
    }
}

struct LenUUIter<'node, I: ItemType> {
    iter: Box<dyn SIterator<I> + 'node>
}

impl<I: ItemType> Iterator for LenUUIter<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<I: ItemType> SIterator<I> for LenUUIter<'_, I> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("lenAM", eval_len_am);
    keywords.insert("lenUF", eval_len_uf);
    keywords.insert("lenUU", eval_len_uu);
}
