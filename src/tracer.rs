use streamlang as stream;

use stream::base::*;
use stream::tracing::*;

use colored::Colorize;

#[derive(Default)]
pub struct TextTracer {
    rec: Vec<PreTraced>,
    tracing: bool,
}

struct PreTraced {
    input: String,
    steps: Vec<Traced>,
}

pub struct Traced {
    input: String,
    steps: Vec<Traced>,
    output: Result<String, String>,
}

impl Tracer for TextTracer {
    fn log(&mut self, ev: Event<'_>) {
        if !self.tracing { return; }
        match ev {
            Event::Enter(node) =>
                self.rec.push(PreTraced {
                    input: node.describe(),
                    steps: Vec::new(),
                }),
            Event::Leave(res) => {
                let pt = self.rec.pop().expect("unbalanced tracer calls");
                let t = Traced {
                    input: pt.input,
                    steps: pt.steps,
                    output: res.as_ref().map(Describe::describe).map_err(ToString::to_string),
                };
                match self.rec.last_mut() {
                    Some(tt) => tt.steps.push(t),
                    None => Self::writeout(t)
                }
            }
        }
    }
}

impl TextTracer {
    fn writeout(t: Traced) {
        Self::writeout_level(t, 0);
    }

    fn writeout_level(t: Traced, level: usize) {
        print!("{}", "  ".repeat(level));
        match t.steps.is_empty() {
            true => print!("{}", Self::styled(&format!("[-] {} => ", t.input))),
            false => {
                println!("{}", Self::styled(&format!("[-] {}", t.input)));
                for tt in t.steps {
                    Self::writeout_level(tt, level + 1);
                }
                print!("{}{}", "  ".repeat(level + 1), Self::styled("=> "));
            }
        }
        match t.output {
            Ok(out) => println!("{}", Self::styled(&out)),
            Err(err) => println!("{}", Self::styled(&format!("[!] {err}"))),
        }
    }

    fn styled(s: &str) -> String {
        s.dimmed().to_string()
    }

    pub(crate) fn toggle(&mut self, on: bool) {
        self.tracing = on;
        if !on {
            self.rec.clear();
        }
    }
}
