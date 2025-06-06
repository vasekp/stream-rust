mod lang;
mod streams;
mod selfref;
mod env;
mod strings;
mod cond;
mod conv;

#[cfg(test)]
mod testutils;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    lang::init(keywords);
    streams::init(keywords);
    selfref::init(keywords);
    env::init(keywords);
    strings::init(keywords);
    cond::init(keywords);
    conv::init(keywords);

    #[cfg(test)]
    testutils::init(keywords);
}
