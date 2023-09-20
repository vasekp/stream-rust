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
        let len = ins.len();
        let mut nums: Vec<TNumber> = vec![];
        for input in ins {
            nums.push(input.imm()?.try_into()?);
        }
        let mut it = nums.into_iter();
        let (from, step) = match len {
            0 => (TNumber::from(1), TNumber::from(1)),
            1 => (it.next().unwrap(), TNumber::from(1)),
            2 => (it.next().unwrap(), it.next().unwrap()),
            _ => return Err(StreamError())
        };
        Ok(IotaStream{from, step}.into_item())
    }
}

impl TStream for IotaStream {
    fn iter(&self) -> Box<dyn Iterator<Item = StreamResult<Item>>> {
        IotaIterator::construct(&self.from, &self.step)
    }
}

impl IotaIterator {
    fn construct(from: &TNumber, step: &TNumber) -> Box<dyn Iterator<Item = StreamResult<Item>>> {
        Box::new(IotaIterator{ value: from.clone(), step: step.clone() })
    }
}

impl Iterator for IotaIterator {
    type Item = StreamResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = Item::from(Into::<Imm>::into(self.value.clone()));
        self.value += &self.step;
        Some(Ok(res))
    }
}
