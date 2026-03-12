use crate::base::*;
use std::marker::PhantomData;

pub(crate) struct Digits<T: TryFrom<UNumber>> {
    powers: Vec<UNumber>,
    queue: Vec<(usize, UNumber)>,
    _phantom: PhantomData<T>,
}

impl<T> Digits<T>
where T: TryFrom<UNumber>, UNumber: From<T> {
    pub fn new(mut num: UNumber, radix: T) -> Self {
        let mut power = UNumber::from(radix);
        let mut powers = Vec::new();
        while power <= num {
            let prev = power;
            power = &prev * &prev;
            powers.push(prev);
        }
        let mut queue = Vec::new();
        for (ix, p) in powers.iter().enumerate().rev() {
            if &num < p { continue; }
            let (quot, div) = num.div_rem(p);
            num = quot;
            queue.push((ix, div));
        }
        queue.push((0, num));
        Self{powers, queue, _phantom: PhantomData}
    }
}

impl<T: TryFrom<UNumber>> Iterator for Digits<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        while let Some((mut ix, q)) = self.queue.pop() {
            if ix == 0 {
                return Some(T::try_from(q).ok().expect("digit should fit in the provided type"));
            } else {
                ix -= 1;
                let (quot, div) = q.div_rem(&self.powers[ix]);
                self.queue.push((ix, div));
                self.queue.push((ix, quot));
            }
        }
        None
    }
}
