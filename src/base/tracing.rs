use crate::base::*;

#[derive(Default)]
pub struct Tracer(Vec<PreTraced>);

struct PreTraced {
    input: String,
    steps: Vec<Traced>,
}

pub struct Traced {
    input: String,
    steps: Vec<Traced>,
    output: Result<String, String>,
}

#[derive(Debug)]
pub enum Event<'a> {
    Enter(&'a Node),
    Leave(&'a Result<Item, StreamError>)
}

impl Tracer {
    pub fn log(&mut self, ev: Event<'_>) {
        match ev {
            Event::Enter(node) =>
                self.0.push(PreTraced {
                    input: node.describe(),
                    steps: Vec::new(),
                }),
            Event::Leave(res) => {
                let pt = self.0.pop().expect("unbalanced tracer calls");
                let t = Traced {
                    input: pt.input,
                    steps: pt.steps,
                    output: res.as_ref().map(Describe::describe).map_err(ToString::to_string),
                };
                match self.0.last_mut() {
                    Some(tt) => tt.steps.push(t),
                    None => Self::writeout(t)
                }
            }
        }
    }

    fn writeout(t: Traced) {
        Self::writeout_level(t, 0);
    }

    fn writeout_level(t: Traced, level: usize) {
        print!("{}", "  ".repeat(level));
        match t.steps.is_empty() {
            true => print!("[-] {} => ", t.input),
            false => {
                println!("[-] {}", t.input);
                for tt in t.steps {
                    Self::writeout_level(tt, level + 1);
                }
                print!("{}=> ", "  ".repeat(level + 1));
            }
        }
        match t.output {
            Ok(out) => println!("{out}"),
            Err(err) => println!("[!] {err}")
        }
    }
}
