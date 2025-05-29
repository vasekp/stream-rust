mod with;
mod alpha;
mod global;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    with::init(keywords);
    alpha::init(keywords);
    global::init(keywords);
}
