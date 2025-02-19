use streamlang as stream;
use stream::base::Describe;
use std::io;

fn main() -> std::io::Result<()> {
    println!("ready >");

    for line in io::stdin().lines() {
        let input = line?;
        match stream::parse(&input) {
            Ok(expr) => {
                println!("Expr Debug: {expr:?}");
                println!("Expr Describe: {}", expr.describe());
                match expr.eval() {
                    Ok(item) => {
                        println!("Item Describe: {}", item.describe());
                        let (s, err) = item.format(Some(80));
                        println!("Item Format: {s}");
                        if let Some(err) = err {
                            println!("Err: {}", err);
                        }
                    },
                    Err(err) => println!("{err}")
                }
            },
            Err(err) => {
                err.display(&input);
                println!("{err}");
            }
        }
        println!();
    }
    Ok(())
}
