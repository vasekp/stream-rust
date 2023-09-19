mod base;
use base::*;

//use itertools::Itertools;

pub fn lib_main() {
    let params = vec![Item::from(1), Item::from(3)];
    let s = IotaStream::construct(params).unwrap();
    println!("{}", s);
}

pub struct IotaStream {
    from: TNumber,
    step: TNumber
}

impl IotaStream {
    fn construct(ins: Vec<Item>) -> StreamResult<Item> {
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
        Box::new(num_iter::range_step(self.from.clone(), (&self.from + TNumber::from(3)* &self.step).clone(), self.step.clone()).map(|x| Ok(Item::from(Into::<Imm>::into(x)))))
    }
}
