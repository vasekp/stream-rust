use streamlang as stream;
use stream::base::*;
use stream::session::*;

use std::io::Write;
use std::process::{Command, Stdio};

use rustyline as rl;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl: rl::Editor<(), _> = rl::Editor::with_history(
    rl::config::Builder::new()
        .auto_add_history(true)
        .build(),
    rl::history::MemHistory::new())?;

    ctrlc::set_handler(stream::base::stop::send_stop)
        .expect("Error setting Ctrl-C handler");

    let mut sess = Session::new();

    while let Ok(input) = rl.readline("> ") {
        if input == "list" {
            let Some(Item::Stream(stm)) = sess.history().last() else {
                println!("Can only use after a stream.");
                continue;
            };
            let mut cmd = Command::new("less")
                .args(["-R"])
                .stdin(Stdio::piped())
                .stdout(Stdio::inherit())
                .spawn()
                .expect("Failed to run less.");
            let mut stdin = cmd.stdin.take().expect("Failed to open stdin");
            for (ix, item) in stm.iter().enumerate() {
                match item {
                    Ok(item) =>
                        if writeln!(stdin, "[{}]  {}", ix + 1, item).is_err() {
                            break;
                        },
                    Err(err) => {
                        let _ = writeln!(stdin, "error: {err}");
                        break;
                    }
                }
            }
            drop(stdin);
            cmd.wait().expect("Error in wait().");
            continue;
        }
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
                        print!("Globals updated: ");
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
