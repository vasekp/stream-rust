use streamlang as stream;
use stream::base::*;
use stream::session::*;
use stream::find_docs;

use std::io::Write;
use std::process::{Command, Stdio};

use rustyline as rl;
use colored::Colorize;

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
        if matches!(&input[..], "list" | "less") {
            let Some(Item::Stream(stm)) = sess.history().last() else {
                println!("{}", "Can only use after a stream.".red());
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
                    Ok(item) => {
                        let index = format!("[{}]", ix + 1).dimmed();
                        if writeln!(stdin, "{}  {}", index, item).is_err() {
                            break;
                        }
                    },
                    Err(err) => {
                        let _ = writeln!(stdin, "{}", format!("error: {err}").red());
                        break;
                    }
                }
            }
            drop(stdin);
            cmd.wait().expect("Error in wait().");
            continue;
        }
        if matches!(&input[0..1], "?") {
            let sym = input[1..].trim();
            // TODO check validity
            if let Some(docs) = find_docs(sym) {
                print!("{}", sym.bold());
                let synonyms = &docs.symbols;
                if synonyms.len() > 1 {
                    print!(" (synonyms: ");
                    let mut iter = synonyms.iter().filter(|x| *x != &sym);
                    if let Some(s) = iter.next() {
                        print!("{}", s);
                    }
                    for s in iter {
                        print!(", {}", s);
                    }
                    print!(")");
                }
                println!();
                if !docs.usage.is_empty() {
                    println!();
                    println!("{}", "Usage:".bright_yellow());
                    for usage in &docs.usage {
                        println!("{}", usage.replace("?", sym));
                    }
                    println!();
                    for s in &docs.desc {
                        println!("{}", s);
                    }
                }
                if !docs.examples.is_empty() {
                    println!();
                    println!("{}", "Examples:".bright_yellow());
                    let mut index = 1;
                    for example in &docs.examples {
                        println!("> {}", example.input.replace("?", sym));
                        match example.output {
                            Ok(out) => {
                                print!("{}", format!("%{}: ", index).dimmed());
                                println!("{}", out);
                                index += 1;
                            },
                            Err(err) => {
                                println!("{}", format!("! {}", err).bright_red());
                            }
                        }
                    }
                }
                let mut iter = docs.see.iter();
                if let Some(see) = iter.next() {
                    println!();
                    print!("{}", "See also: ".bright_yellow());
                    print!("{}", see);
                    for see in iter {
                        print!(", {}", see);
                    }
                    println!();
                }
            } else {
                println!("{}", format!("No documentation found for '{sym}'").red());
            }
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
                        println!("{} {s}", format!("%{index}:").dimmed());
                        if let Some(err) = err {
                            println!("{}", format!("{err}").red());
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
                    Err(err) => println!("{}", format!("{err}").red())
                }
            },
            Err(err) => {
                err.display(&input);
                println!("{}", format!("{err}").red());
            }
        }
        //println!();
    }
    Ok(())
}
