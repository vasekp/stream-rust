mod list;
mod part;
mod map;
mod mathops;
mod args;
mod cmp;
mod lexcmp;
mod bools;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    list::init(keywords);
    part::init(keywords);
    map::init(keywords);
    mathops::init(keywords);
    args::init(keywords);
    cmp::init(keywords);
    lexcmp::init(keywords);
    bools::init(keywords);
}
