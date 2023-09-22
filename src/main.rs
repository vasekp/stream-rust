use streamlang::*;

fn main() {
    let params = vec![Item::new_imm(1), Item::new_imm(3)];
    let s = IotaStream::construct(params).unwrap();
    println!("{:.80}", s);
}
