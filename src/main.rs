use streamlang as stream;
use stream::base::*;
use stream::session::*;
use stream::find_docs;
use stream::docs;

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
    let tracer = Rc::new(std::cell::RefCell::new(tracer::TextTracer::default()));
    sess.set_tracer(Rc::clone(&tracer));

    while let Ok(input) = rl.readline("> ") {
        let input = input.trim();
        if input.is_empty() { continue; }
        if input.as_bytes()[0] == b':' {
            let mut iter = input[1..].split(' ').filter(|s| !s.is_empty());
            match iter.next() {
                Some("list") | Some("less") => {
                    if iter.next().is_some() {
                        eprintln!("{}", "invalid command".red());
                        continue;
                    }
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
                Some("trace") | Some("tracing") => {
                    let state = match iter.next() {
                        Some("on") | None => true,
                        Some("off") => false,
                        _ => {
                            eprintln!("{}", "invalid command".red());
                            continue;
                        }
                    };
                    tracer.borrow_mut().toggle(state);
                },
                Some("vars") => {
                    if iter.next().is_some() {
                        eprintln!("{}", "invalid command".red());
                        continue;
                    }
                    for (name, rhs) in sess.vars() {
                        println!("{}", format!("{} = {}", name, rhs.describe()).yellow());
                    }
                },
                Some("show") => {
                    let (Some(var), None) = (iter.next(), iter.next()) else {
                        eprintln!("{}", "malformed command".red());
                        continue;
                    };
                    // TODO check name
                    let Some(rhs) = sess.vars().get(var) else {
                        eprintln!("{}", "not defined".red());
                        continue;
                    };
                    println!("{}", format!("{} = {}", var, rhs.describe()).yellow());
                },
                Some("desc" | "describe") => {
                    if iter.next().is_some() {
                        eprintln!("{}", "invalid command".red());
                        continue;
                    }
                    let Some(item) = sess.history().last() else {
                        println!("{}", "History is empty.".red());
                        continue;
                    };
                    println!("{}", item.describe());
                },
                None => eprintln!("{}", "malformed command".red()),
                _ => eprintln!("{}", "unknown command".red()),
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
                        println!("{}", format_cli(usage, sym).white());
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
                        println!("> {}", format_cli(example.input, sym).white());
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
                        print!(", {}", see.white().underline());
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
                match sess.process(expr) {
                    Ok(SessionUpdate::History(index, item)) => {
                        let (s, _, err) = item.format(None, Some(80));
                        println!("{} {s}", format!("%{index}:").dimmed());
                        if let Some(err) = err {
                            println!("{}", format!("{err}").red());
                        }
                    },
                    Ok(SessionUpdate::Globals(list)) => {
                        for name in list {
                            match sess.vars().get(&name) {
                                Some(rhs) => println!("{}", format!("{} = {}", name, rhs.describe()).yellow()),
                                None => println!("{}", name.yellow().strikethrough()),
                            }
                        }
                    },
                    Err(err) => println!("{}", format!("{err}").red())
                }
            },
            Err(err) => {
                err.display(input);
                println!("{}", format!("{err}").red());
            }
        }
    }
    Ok(())
}

fn format_cli(line: &str, sym: &str) -> String {
    use docs::{LinePart, RefStringItem};
    let mut ret = String::new();
    for LinePart { content, is_code } in docs::parse_line(line, sym) {
        let mut part = String::new();
        for item in content {
            match item {
                RefStringItem::Base(s) => part += &s,
                RefStringItem::Ref(s) => part += &s.white().underline().to_string(),
            }
        }
        if is_code {
            ret += &part.white().to_string();
        } else {
            ret += &part.to_string();
        }
    }
    ret
}
