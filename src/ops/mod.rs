mod lang;
mod streams;
pub(crate) mod selfref;
mod with;
mod alpha;

#[cfg(test)]
mod testutils;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    lang::init(keywords);
    streams::init(keywords);
    selfref::init(keywords);
    with::init(keywords);
    alpha::init(keywords);

    #[cfg(test)]
    testutils::init(keywords);
}
