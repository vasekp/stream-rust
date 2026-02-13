#[derive(Default)]
pub struct DocRecord {
    pub desc: Vec<&'static str>,
    pub symbols: Vec<&'static str>,
    pub usage: Vec<&'static str>,
    pub examples: Vec<Example>,
    pub see: Vec<&'static str>,
}

pub struct Example {
    pub input: &'static str,
    pub width: Option<usize>,
    pub output: Result<&'static str, &'static str>,
}

pub(crate) fn parse_docs(input: &'static str) -> DocRecord {
    let mut rec = DocRecord::default();
    for line in input.lines() {
        if line.is_empty() { continue; }
        match &line[0..1] {
            "-" => rec.desc.push(line[1..].trim()),
            "=" => rec.usage.push(line[1..].trim()),
            ">" => rec.examples.push(parse_example(line[1..].trim())),
            ":" => rec.see.push(line[1..].trim()),
            _ => rec.desc.push(line),
        }
    }
    rec
}

fn parse_example(line: &'static str) -> Example {
    let (first, second) = line.split_once(" => ").expect("{line} must respect the format 'input => output'");
    let (input, width) = match first.split_once(" : ") {
        Some((input, width)) => (input, Some(width.parse().expect("error parsing width specification in {first}"))),
        None => (first, None)
    };
    let output = if &second[0..1] == "!" { Err(&second[1..]) } else { Ok(second) };
    Example{input, width, output}
}

#[derive(Debug, PartialEq)]
pub enum RefStringItem {
    Base(String),
    Ref(String)
}

#[derive(Debug, PartialEq)]
pub struct LinePart {
    pub content: Vec<RefStringItem>,
    pub is_code: bool,
}

pub fn parse_line(line: &str, sym: &str) -> Vec<LinePart> {
    let mut iter = line.chars().peekable();
    let mut partial = String::with_capacity(line.len());
    let mut out = Vec::new();
    let mut part = LinePart { content: Vec::new(), is_code: false };
    while let Some(c) = iter.next() {
        match c {
            '?' => {
                if !partial.is_empty() {
                    part.content.push(RefStringItem::Base(std::mem::take(&mut partial)));
                }
                let mut s2 = String::with_capacity(10);
                while let Some(c2) = iter.peek() {
                    if c2.is_ascii_alphanumeric() {
                        s2.push(*c2);
                        iter.next();
                    } else {
                        break;
                    }
                }
                if s2.is_empty() {
                    part.content.push(RefStringItem::Base(sym.to_string()));
                } else {
                    part.content.push(RefStringItem::Ref(s2));
                }
            },
            '`' => {
                if !partial.is_empty() {
                    part.content.push(RefStringItem::Base(std::mem::take(&mut partial)));
                }
                if !part.content.is_empty() {
                    out.push(LinePart { content: std::mem::take(&mut part.content), is_code: part.is_code });
                }
                part.is_code = !part.is_code;
            },
            _ => partial.push(c)
        }
    }
    if !partial.is_empty() {
        part.content.push(RefStringItem::Base(std::mem::take(&mut partial)));
    }
    if part.is_code {
        panic!("unterminated '`' in doc string of {sym}");
    } else {
        if !part.content.is_empty() {
            out.push(LinePart { content: std::mem::take(&mut part.content), is_code: part.is_code });
        }
    }
    out
}

impl LinePart {
    pub fn flatten(&self) -> String {
        let mut ret = String::new();
        for item in &self.content {
            match item {
                RefStringItem::Base(s) | RefStringItem::Ref(s) => ret += s
            }
        }
        ret
    }
}

#[cfg(test)]
#[test]
fn test_parse_line() {
    assert_eq!(parse_line("abc?def`ghi?jkl`mno", "sym"), vec![
        LinePart { content: vec![ RefStringItem::Base("abc".to_string()), RefStringItem::Ref("def".to_string()) ], is_code: false },
        LinePart { content: vec![ RefStringItem::Base("ghi".to_string()), RefStringItem::Ref("jkl".to_string()) ], is_code: true },
        LinePart { content: vec![ RefStringItem::Base("mno".to_string()) ], is_code: false },
    ]);
    assert_eq!(parse_line("?abc def`?ghi jkl`", "sym"), vec![
        LinePart { content: vec![ RefStringItem::Ref("abc".to_string()), RefStringItem::Base(" def".to_string()) ], is_code: false },
        LinePart { content: vec![ RefStringItem::Ref("ghi".to_string()), RefStringItem::Base(" jkl".to_string()) ], is_code: true },
    ]);
    assert_eq!(parse_line("`abc``def`ghi``jkl?", "sym"), vec![
        LinePart { content: vec![ RefStringItem::Base("abc".to_string()) ], is_code: true },
        LinePart { content: vec![ RefStringItem::Base("def".to_string()) ], is_code: true },
        LinePart { content: vec![ RefStringItem::Base("ghi".to_string()) ], is_code: false },
        LinePart { content: vec![ RefStringItem::Base("jkl".to_string()), RefStringItem::Base("sym".to_string()) ], is_code: false },
    ]);
}

#[cfg(test)]
#[test]
fn test_flatten() {
    let [line] = &parse_line("test ?ref ?()", "sym")[..] else { panic!("should return only 1 item") };
    assert_eq!(line.flatten(), "test ref sym()");
}
