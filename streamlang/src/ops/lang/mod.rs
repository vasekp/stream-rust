mod list;
mod part;
//mod map;
mod mathops;
mod args;
mod cmp;
mod assign;
mod lexcmp;
mod bools;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    list::init(symbols);
    part::init(symbols);
    //map::init(symbols);
    mathops::init(symbols);
    args::init(symbols);
    cmp::init(symbols);
    assign::init(symbols);
    lexcmp::init(symbols);
    bools::init(symbols);
}
