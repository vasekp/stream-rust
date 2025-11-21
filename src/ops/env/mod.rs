mod with;
mod alpha;
mod global;

pub(crate) fn init(symbols: &mut crate::symbols::Symbols) {
    with::init(symbols);
    alpha::init(symbols);
    global::init(symbols);
}
