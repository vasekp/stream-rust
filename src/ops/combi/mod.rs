mod reorder;
mod perm;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    reorder::init(keywords);
    perm::init(keywords);
}
