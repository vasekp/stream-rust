mod base;
mod ops;

//use itertools::Itertools;

pub fn lib_main() {
    let params = vec![base::Item::from(1), base::Item::from(3)];
    let s = ops::IotaStream::construct(params).unwrap();
    println!("{}", s);
}
