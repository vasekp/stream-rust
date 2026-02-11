use streamlang as stream;
use stream::base::*;
use stream::session::*;
use stream::find_docs;

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
        } else if input.as_bytes()[0] == b'?' {
            let sym = input[1..].trim();
            // TODO check validity
            if let Some(docs) = find_docs(sym) {
                print!("{}", sym.white().bold());
                let synonyms = &docs.symbols;
                if synonyms.len() > 1 {
                    print!(" (synonyms: ");
                    let mut iter = synonyms.iter().filter(|x| *x != &sym);
                    if let Some(s) = iter.next() {
                        print!("{}", s.white());
                    }
                    for s in iter {
                        print!(", {}", s.white());
                    }
                    print!(")");
                }
                println!();
                if !docs.usage.is_empty() {
                    println!();
                    println!("{}", "Usage:".yellow().bold());
                    for usage in &docs.usage {
                        println!("{}", format_cli(&usage, &sym).white());
                    }
                }
                println!();
                for s in &docs.desc {
                    println!("{}", format_cli(s, sym));
                }
                if !docs.examples.is_empty() {
                    println!();
                    println!("{}", "Examples:".yellow().bold());
                    let mut index = 1;
                    for example in &docs.examples {
                        println!("> {}", format_cli(example.input, &sym).white());
                        match example.output {
                            Ok(out) => {
                                println!("{}",
                                    format!("{} {}", format!("%{}:", index).dimmed(), out).white());
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
                    print!("{}", "See also: ".yellow().bold());
                    print!("{}", see.white().underline());
                    for see in iter {
                        print!(", {}", see.underline());
                    }
                    println!();
                }
            } else {
                println!("{}", format!("No documentation found for '{sym}'").red());
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

fn format_cli(s: &str, sym: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut partial = String::with_capacity(s.len());
    let mut iter = s.chars().peekable();
    let mut in_code = false;
    while let Some(c) = iter.next() {
        match c {
            '?' => {
                let mut s2 = String::with_capacity(10);
                if iter.peek() == Some(&'?') {
                    iter.next();
                    partial += &sym.white().to_string();
                    continue;
                }
                while let Some(c2) = iter.peek() {
                    if c2.is_ascii_alphanumeric() {
                        s2.push(*c2);
                        iter.next();
                    } else {
                        break;
                    }
                }
                if s2.is_empty() {
                    panic!("invalid docstring for {sym}: ?");
                } else {
                    partial += &s2.white().underline().to_string();
                }
            },
            '`' => {
                if in_code {
                    out += &partial.white().to_string();
                } else {
                    out += &partial;
                }
                in_code = !in_code;
                partial.clear();
            },
            _ => partial.push(c)
        }
    }
    if in_code {
        panic!("unterminated '`' in doc string of {sym}");
    } else {
        out += &partial;
    }
    out
}
