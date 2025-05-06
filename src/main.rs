use streamlang as stream;
use stream::base::Describe;
use rustyline as rl;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl: rl::Editor<(), _> = rl::Editor::with_history(
    rl::config::Builder::new()
        .auto_add_history(true)
        .build(),
    rl::history::MemHistory::new())?;

    while let Ok(input) = rl.readline("> ") {
        match stream::parse(&input) {
            Ok(expr) => {
                println!("Expr Debug: {expr:?}");
                println!("Expr Describe: {}", expr.describe());
                match expr.eval_default() {
                    Ok(item) => {
                        println!("Item Describe: {}", item.describe());
                        let (s, _, err) = item.format(None, Some(80));
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
