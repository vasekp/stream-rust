mod lang;
mod streams;
mod strings;
pub(crate) mod selfref;

#[cfg(test)]
mod testutils;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    lang::init(keywords);
    streams::init(keywords);
    strings::init(keywords);
    selfref::init(keywords);

    #[cfg(test)]
    testutils::init(keywords);
}
