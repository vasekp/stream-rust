use streamlang as stream;
use std::io;

fn main() {
    let session = stream::Session::new();
    println!("ready >");

    let mut buffer = String::new();
    let stdin = io::stdin();
    while let Ok(len) = stdin.read_line(&mut buffer) {
        if len == 0 {
            break;
        }
        let input = buffer.trim();
        match stream::parse(input) {
            Ok(expr) => {
                println!("{expr:?}");
                match session.eval(&expr) {
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
