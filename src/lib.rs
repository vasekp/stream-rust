pub mod base;
mod parser;
mod symbols;
pub mod docs;
mod utils;
mod ops;
pub mod session;

pub use parser::parse;

pub fn find_docs(name: &str) -> Option<&docs::DocRecord> {
    symbols::Symbols::find_docs(name)
}
