mod reorder;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    reorder::init(keywords);
}
