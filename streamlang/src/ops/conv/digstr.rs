use crate::base::*;
use super::digits::*;

fn eval_digstr(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.as_stream()?;
    node.check_no_args()?;
    Ok(Item::new_string(DigStr{source: Rc::clone(stm), head: node.head.clone()}))
}

struct DigStr {
    source: Rc<dyn Stream>,
    head: Head,
}

impl Describe for DigStr {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream<Char> for DigStr {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<Char>> {
        Box::new(SMap::new(
            &self.source,
            |item: Item| dig_to_char(item.as_num()?.try_cast()?).map(Char::from),
            &self
        ))
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

fn eval_strdig(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.as_char_stream()?;
    node.check_no_args()?;
    Ok(Item::new_stream(StrDig{source: Rc::clone(stm), head: node.head.clone()}))
}

struct StrDig {
    source: Rc<dyn Stream<Char>>,
    head: Head,
}

impl Describe for StrDig {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for StrDig {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        Box::new(SMap::new(
            &self.source,
            |ch: Char| match ch {
                Char::Single(ch) => char_to_dig(ch).map(Item::new_number),
                Char::Multi(_) => Err("invalid digit".into()),
            },
            &self
        ))
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_digstr() {
        use super::*;
        test_eval!("seq(1,3).digstr" => "\"147ADGJMPSVY<!>");
        test_eval!("(34..36).digstr" => "\"YZ<!>");
        test_eval!("range(1,-1,-1).digstr" => "\"10<!>");
    }

    #[test]
    fn test_strdig() {
        use super::*;
        test_eval!("\"012xyZ\".strdig" : 10 => "[0, 1, 2, 33, 34, 35]");
        test_eval!("\"1 2\".strdig" : 10 => "[1, <!>");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("digstr", eval_digstr, r#"
Converts a stream of digits into a string.
= stream.?
> [1, 2, 15].? => "12F"
: strdig
: dignum
: numstr
: digits
"#);
    symbols.insert("strdig", eval_strdig, r#"
Converts a string of digit characters into a stream of numeric digits.
= stream.?
> "12F".? => [1, 2, 15]
: digstr
: strnum
: numdig
"#);
}
