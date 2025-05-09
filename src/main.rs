use streamlang as stream;
use stream::base::*;
use stream::session::*;

use rustyline as rl;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl: rl::Editor<(), _> = rl::Editor::with_history(
    rl::config::Builder::new()
        .auto_add_history(true)
        .build(),
    rl::history::MemHistory::new())?;

    let mut sess = Session::new();

    while let Ok(input) = rl.readline("> ") {
        match stream::parse(&input) {
            Ok(expr) => {
                //println!("Expr Debug: {expr:?}");
                //println!("Expr Describe: {}", expr.describe());
                match sess.process(expr) {
                    Ok(SessionUpdate::History(index, item)) => {
                        println!("Item Describe: {}", item.describe());
                        let (s, _, err) = item.format(None, Some(80));
                        println!("%{index}: {s}");
                        if let Some(err) = err {
                            println!("{err}");
                        }
                    },
                    Ok(SessionUpdate::Globals(list)) => {
                        print!("Globals updates: ");
                        let mut iter = list.into_iter();
                        if let Some(first) = iter.next() {
                            print!("{first}");
                        }
                        for name in iter {
                            print!(", {name}");
                        }
                        println!();
                    },
                    Err(err) => println!("{err}")
                }
            },
            Err(err) => {
                err.display(&input);
                println!("{err}");
            }
        }
        //println!();
    }
    Ok(())
}
