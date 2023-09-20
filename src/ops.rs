use crate::base::*;

pub struct IotaStream {
    from: TNumber,
    step: TNumber
}

struct IotaIterator {
    value: TNumber,
    step: TNumber
}

impl IotaStream {
    pub fn construct(ins: Vec<Item>) -> StreamResult<Item> {
        if ins.len() > 2 {
            return Err(StreamError())
        }
        let mut nums: Vec<TNumber> = vec![];
        for input in ins {
            nums.push(input.into_num()?)
        }
        let mut it = nums.into_iter();
        let from = it.next().unwrap_or(TNumber::from(1));
        let step = it.next().unwrap_or(TNumber::from(1));
        Ok(Item::new_stream(IotaStream{from, step}))
    }
}

impl TStream for IotaStream {
    fn iter(&self) -> Box<dyn Iterator<Item = StreamResult<Item>>> {
        Box::new(IotaIterator::new(&self.from, &self.step))
    }
}

impl IotaIterator {
    fn new(from: &TNumber, step: &TNumber) -> IotaIterator {
        IotaIterator{ value: from.clone(), step: step.clone() }
    }
}

impl Iterator for IotaIterator {
    type Item = StreamResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = Item::new_imm(self.value.clone());
        self.value += &self.step;
        Some(Ok(res))
    }
}
