use crate::base::*;

fn eval_len_am(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    match node.source_checked()? {
        Item::Stream(stm) => Ok(Item::new_stream(LenAM { head: node.head.clone(), src: Rc::clone(stm) })),
        Item::String(stm) => Ok(Item::new_string(LenAM { head: node.head.clone(), src: Rc::clone(stm) })),
        _ => panic!()
    }
}

struct LenAM<I: ItemType> {
    head: Head,
    src: Rc<dyn Stream<I>>,
}

impl<I: ItemType> Describe for LenAM<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.src)
            .finish(prec)
    }
}

impl<I: ItemType> Stream<I> for LenAM<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        Box::new(LenAMIter { iter: self.src.iter() })
    }

    fn len(&self) -> Length {
        Length::at_most(self.src.len())
    }
}

struct LenAMIter<I: ItemType> {
    iter: Box<dyn SIterator<I>>
}

impl<I: ItemType> SIterator<I> for LenAMIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        self.iter.next()
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.iter.len_remain())
    }
}

fn eval_len_uf(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    match node.source_checked()? {
        Item::Stream(stm) => Ok(Item::new_stream(LenUF { head: node.head.clone(), src: Rc::clone(stm) })),
        Item::String(stm) => Ok(Item::new_string(LenUF { head: node.head.clone(), src: Rc::clone(stm) })),
        _ => panic!()
    }
}

struct LenUF<I: ItemType> {
    head: Head,
    src: Rc<dyn Stream<I>>,
}

impl<I: ItemType> Describe for LenUF<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.src)
            .finish(prec)
    }
}

impl<I: ItemType> Stream<I> for LenUF<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        Box::new(LenUFIter { iter: self.src.iter() })
    }

    fn len(&self) -> Length {
        Length::UnknownFinite
    }
}

struct LenUFIter<I: ItemType> {
    iter: Box<dyn SIterator<I>>
}

impl<I: ItemType> SIterator<I> for LenUFIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        self.iter.next()
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        Length::UnknownFinite
    }
}

fn eval_len_uu(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    match node.source_checked()? {
        Item::Stream(stm) => Ok(Item::new_stream(LenUU { head: node.head.clone(), src: Rc::clone(stm) })),
        Item::String(stm) => Ok(Item::new_string(LenUU { head: node.head.clone(), src: Rc::clone(stm) })),
        _ => panic!()
    }
}

struct LenUU<I: ItemType> {
    head: Head,
    src: Rc<dyn Stream<I>>,
}

impl<I: ItemType> Describe for LenUU<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.src)
            .finish(prec)
    }
}

impl<I: ItemType> Stream<I> for LenUU<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        Box::new(LenUUIter { iter: self.src.iter() })
    }

    fn len(&self) -> Length {
        Length::Unknown
    }
}

struct LenUUIter<I: ItemType> {
    iter: Box<dyn SIterator<I>>
}

impl<I: ItemType> SIterator<I> for LenUUIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        self.iter.next()
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_raw("$lenAM", eval_len_am);
    symbols.insert_raw("$lenUF", eval_len_uf);
    symbols.insert_raw("$lenUU", eval_len_uu);
}
