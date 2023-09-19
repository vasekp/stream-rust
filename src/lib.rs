mod base;
use base::*;

pub struct IotaStream();

impl TStream for IotaStream {
    fn iter(&self) -> Box<dyn TIterator> {
        let a = "10000000000000000000000000".parse::<TNumber>().unwrap();
        let b = TNumber::from(10);
        let c = &a + &b + &b;
        Box::new(num_iter::range_step(a, c, b))
    }
}

pub fn create_iota() -> Item {
    IotaStream().as_item()
}
