mod reorder;
mod perm;
mod factorial;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    reorder::init(keywords);
    perm::init(keywords);
    factorial::init(keywords);
}
