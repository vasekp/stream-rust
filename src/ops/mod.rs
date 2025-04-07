mod lang;
mod streams;
mod strings;
pub(crate) mod selfref;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    lang::init(keywords);
    streams::init(keywords);
    strings::init(keywords);
    selfref::init(keywords);
}
