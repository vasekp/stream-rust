use streamlang::*;
use std::io;

fn main() {
    /*let params = vec![Item::new_atomic(1), Item::new_atomic(3)];
    let s = RangeStream::construct(params).unwrap();
    println!("{:.80?}", s);*/

    println!("ready >");

    let mut buffer = String::new();
    let stdin = io::stdin();
    while let Ok(len) = stdin.read_line(&mut buffer) {
        if len == 0 {
            break;
        }
        let input = buffer.trim();
        match parse(input) {
            Ok(expr) => println!("{expr:#?}"),
            Err(err) => {
                err.display(input);
                println!("{err}");
            }
        }
        buffer.clear();
        println!();
    }
}
