mod with;
mod alpha;

pub(crate) fn init(symbols: &mut crate::symbols::Symbols) {
    with::init(symbols);
    alpha::init(symbols);
}
