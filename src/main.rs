use streamlang::*;
use std::io;

fn main() {
    /*let params = vec![Item::new_atomic(1), Item::new_atomic(3)];
    let s = RangeStream::construct(params).unwrap();
    println!("{:.80?}", s);*/

    /*let list = Item::new_stream(vec![Item::new_atomic(1), Item::new_atomic(2)]);
    println!("{list:?}");*/

    println!("ready >");

    let mut buffer = String::new();
    let stdin = io::stdin();
    while let Ok(len) = stdin.read_line(&mut buffer) {
        if len == 0 {
            break;
        }
        let input = buffer.trim();
        match parse(input) {
            Ok(expr) => {
                match expr.eval() {
                    Ok(item) => println!("{item:.80}"),
                    Err(err) => println!("{err}")
                }
            },
            Err(err) => {
                err.display(input);
                println!("{err}");
            }
        }
        buffer.clear();
        println!();
    }
}
