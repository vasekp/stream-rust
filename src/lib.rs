pub mod base;
pub mod ops;

pub fn lib_main() {
    let params = vec![base::Item::new_imm(1), base::Item::new_imm(3)];
    let s = ops::IotaStream::construct(params).unwrap();
    println!("{:.80}", s);
}
