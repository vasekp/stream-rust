use streamlang as stream;
use stream::base::*;
use stream::session::*;

use std::io::Write;
use std::process::{Command, Stdio};

use rustyline as rl;
use colored::Colorize;

mod tracer;

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
        let input = input.trim();
        if input.as_bytes()[0] == b':' {
            match input[1..].trim() {
                "list" | "less" => {
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
                },
                "trace" | "trace on" | "tracing" | "tracing on" =>
                    sess.set_tracer(tracer::TextTracer::default()),
                "trace off" | "tracing off" =>
                    sess.set_tracer(()),
                _ => eprintln!("{}", "malformed command".red()),
            }
            continue;
        }
        match stream::parse(input) {
            Ok(expr) => {
                //println!("Expr Debug: {expr:?}");
                //println!("Expr Describe: {}", expr.describe());
                match sess.process(expr) {
                    Ok(SessionUpdate::History(index, item)) => {
                        //println!("Item Describe: {}", item.describe());
                        let (s, _, err) = item.format(None, Some(80));
                        println!("{} {s}", format!("%{index}:").dimmed());
                        if let Some(err) = err {
                            println!("{}", format!("{err}").red());
                        }
                    },
                    Ok(SessionUpdate::Globals(list)) => {
                        let mut s = "Globals updated: ".to_string();
                        let mut iter = list.into_iter();
                        if let Some(first) = iter.next() {
                            s += &first.to_string();
                        }
                        for name in iter {
                            s += &format!(", {name}");
                        }
                        println!("{}", s.yellow());
                    },
                    Err(err) => println!("{}", format!("{err}").red())
                }
            },
            Err(err) => {
                err.display(input);
                println!("{}", format!("{err}").red());
            }
        }
        //println!();
    }
    Ok(())
}
