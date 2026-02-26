mod reorder;
mod perm;
mod factorial;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    reorder::init(symbols);
    perm::init(symbols);
    factorial::init(symbols);
}
