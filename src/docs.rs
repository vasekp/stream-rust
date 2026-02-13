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

pub enum ChunkType {
    Base,
    Code,
    Ref
}

pub fn parse_line(line: &str, sym: &str) -> Vec<(ChunkType, String)> {
    let mut iter = line.chars().peekable();
    let mut partial = String::with_capacity(line.len());
    let mut in_code = false;
    let mut out = Vec::new();
    while let Some(c) = iter.next() {
        match c {
            '?' => {
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
                    out.push((ChunkType::Ref, sym.to_string()))
                } else {
                    out.push((ChunkType::Ref, s2))
                }
            },
            '`' => {
                out.push((if in_code { ChunkType::Code } else { ChunkType::Base }, std::mem::take(&mut partial)));
                in_code = !in_code;
            },
            _ => partial.push(c)
        }
    }
    if in_code {
        panic!("unterminated '`' in doc string of {sym}");
    } else {
        out.push((ChunkType::Base, partial));
    }
    out
}
