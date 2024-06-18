use streamlang as stream;
use stream::base::Describe;
use std::io;

fn main() {
    println!("ready >");

    let mut buffer = String::new();
    let stdin = io::stdin();
    let env = Default::default();
    while let Ok(len) = stdin.read_line(&mut buffer) {
        if len == 0 {
            break;
        }
        let input = buffer.trim();
        match stream::parse(input) {
            Ok(expr) => {
                println!("Expr Debug: {expr:?}");
                println!("Expr Describe: {}", expr.describe());
                match expr.eval(&env) {
                    Ok(item) => {
                        println!("Item Describe: {}", item.describe());
                        let (s, err) = item.format(80);
                        println!("Item Format: {s}");
                        if err.is_some() {
                            println!("Err: {}", err.unwrap());
                        }
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
