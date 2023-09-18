use streamlang::Node;

fn main() {
    let s = Node::parse("iota");
    println!("{}", s.unwrap());
}
