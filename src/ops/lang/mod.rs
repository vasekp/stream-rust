mod list;
mod part;
mod map;
mod mathops;
mod join;
mod args;
mod cmp;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    list::init(keywords);
    part::init(keywords);
    map::init(keywords);
    mathops::init(keywords);
    join::init(keywords);
    args::init(keywords);
    cmp::init(keywords);
}
