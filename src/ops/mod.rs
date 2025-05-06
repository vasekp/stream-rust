mod lang;
mod streams;
mod strings;
pub(crate) mod selfref;
mod with;

#[cfg(test)]
mod testutils;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    lang::init(keywords);
    streams::init(keywords);
    strings::init(keywords);
    selfref::init(keywords);
    with::init(keywords);

    #[cfg(test)]
    testutils::init(keywords);
}
