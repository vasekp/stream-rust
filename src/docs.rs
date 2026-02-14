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
    let (first, second) = line.split_once(" => ").expect("{line} must respect the format 'lineput => output'");
    let (input, width) = match first.split_once(" : ") {
        Some((input, width)) => (input, Some(width.parse().expect("error parsing width specification in {first}"))),
        None => (first, None)
    };
    let output = if &second[0..1] == "!" { Err(&second[1..]) } else { Ok(second) };
    Example{input, width, output}
}

#[derive(Debug, PartialEq)]
pub enum RefStringItem<'line> {
    Base(&'line str),
    Ref(&'line str)
}

#[derive(Debug, PartialEq)]
pub struct LinePart<'line> {
    pub content: Vec<RefStringItem<'line>>,
    pub is_code: bool,
}

pub fn parse_line<'line>(line: &'line str, sym: &'line str) -> Vec<LinePart<'line>> {
    let mut iter = line.char_indices().peekable();
    let mut out = Vec::new();
    let mut part = LinePart { content: Vec::new(), is_code: false };
    let mut last_pos = 0;
    while let Some((pos, c)) = iter.next() {
        match c {
            '?' => {
                if pos != last_pos {
                    part.content.push(RefStringItem::Base(&line[last_pos..pos]));
                }
                last_pos = pos + 1;
                while iter.next_if(|(_, c)| c.is_ascii_alphanumeric()).is_some() { }
                let pos = iter.peek().map(|(pos, _)| *pos).unwrap_or_else(|| line.len());
                if pos == last_pos {
                    part.content.push(RefStringItem::Base(sym));
                } else {
                    part.content.push(RefStringItem::Ref(&line[last_pos..pos]));
                    last_pos = pos;
                }
            },
            '`' => {
                if pos != last_pos {
                    part.content.push(RefStringItem::Base(&line[last_pos..pos]));
                }
                last_pos = pos + 1;
                if !part.content.is_empty() {
                    out.push(LinePart { content: std::mem::take(&mut part.content), is_code: part.is_code });
                }
                part.is_code = !part.is_code;
            },
            _ => ()
        }
    }
    if last_pos < line.len() {
        part.content.push(RefStringItem::Base(&line[last_pos..]));
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

impl LinePart<'_> {
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
    assert_eq!(parse_line("abc?def", "sym"), vec![
        LinePart { content: vec![ RefStringItem::Base("abc"), RefStringItem::Ref("def") ], is_code: false },
    ]);
    assert_eq!(parse_line("abc?def`ghi?jkl`mno", "sym"), vec![
        LinePart { content: vec![ RefStringItem::Base("abc"), RefStringItem::Ref("def") ], is_code: false },
        LinePart { content: vec![ RefStringItem::Base("ghi"), RefStringItem::Ref("jkl") ], is_code: true },
        LinePart { content: vec![ RefStringItem::Base("mno") ], is_code: false },
    ]);
    assert_eq!(parse_line("?abc def`?ghi jkl`", "sym"), vec![
        LinePart { content: vec![ RefStringItem::Ref("abc"), RefStringItem::Base(" def") ], is_code: false },
        LinePart { content: vec![ RefStringItem::Ref("ghi"), RefStringItem::Base(" jkl") ], is_code: true },
    ]);
    assert_eq!(parse_line("`abc``def`ghi``jkl?", "sym"), vec![
        LinePart { content: vec![ RefStringItem::Base("abc") ], is_code: true },
        LinePart { content: vec![ RefStringItem::Base("def") ], is_code: true },
        LinePart { content: vec![ RefStringItem::Base("ghi") ], is_code: false },
        LinePart { content: vec![ RefStringItem::Base("jkl"), RefStringItem::Base("sym") ], is_code: false },
    ]);
}

#[cfg(test)]
#[test]
fn test_flatten() {
    let [line] = &parse_line("test ?ref ?()", "sym")[..] else { panic!("should return only 1 item") };
    assert_eq!(line.flatten(), "test ref sym()");
}
