use streamlang as stream;
use stream::base::Describe;
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
                println!("Expr Debug: {expr:?}");
                println!("Expr Describe: {}", expr.describe());
                match session.eval(expr) {
                    Ok(item) => {
                        println!("Item Describe: {}", item.describe());
                        println!("Item Display: {item:.80}");
                    },
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
