use crate::docs::DocRecord;
use std::collections::HashMap;
use std::sync::LazyLock;

/// Help pages on the language itself, which is not bound to any symbol. They are looked up
/// under an empty name (the quick guide) or under names starting with `?` (its sections).
pub(crate) struct Topics(HashMap<&'static str, DocRecord>);

static TOPICS: LazyLock<Topics> = LazyLock::new(|| {
    let mut topics = Topics(HashMap::new());
    init(&mut topics);
    topics
});

impl Topics {
    fn insert(&mut self, name: &'static str, title: &'static str, doc_string: &'static str) {
        let mut docs = DocRecord::from(doc_string);
        docs.title = Some(title);
        let prev = self.0.insert(name, docs);
        if prev.is_some() {
            panic!("Duplicate definition of help topic {name:?}");
        }
    }

    pub(crate) fn find_docs(name: &str) -> Option<&'static DocRecord> {
        TOPICS.0.get(name)
    }
}

#[rustfmt::skip]
fn init(topics: &mut Topics) {
    topics.insert("", "Stream quick guide", r#"
Stream processes numbers, characters, strings and streams of them by chains of operations read from left to right.
Every input is one expression. Its result is stored under the number shown, and `%` refers to the last one.
* Type `?` followed by the name of a function or an operator for its documentation, or `??` followed by one of the topics below.
> 2^64 - 1 => 18446744073709551615 ; numbers are integers of any size
> [1, 2, 3] * 10 + 1 => [11, 21, 31] ; arithmetic works itemwise on streams
> ?range(5).?rev => [5, 4, 3, 2, 1] ; `source.function(arguments)`
> ?seq => [1, 2, 3, 4, 5, ...] ; streams can be infinite
> ?seq:{#^2} => [1, 4, 9, 16, 25, ...] ; `:` applies a function to every item, `{...}` writes one in place
> ?primes.?select{# % 4 == 1}.?first(3) => [5, 13, 17]
> ?primes[100] => 541 ; parts are counted from 1
> "Hello, world!" + 1 => "Ifmmp, xpsme!" ; letters shift in the alphabet
> "Stream".?chars:?ord : 10 => [19, 20, 18, 5, 1, 13]
> "mississippi".?counts : 20 => [['m', 1], ['i', 4], ['s', 4], ['p', 2]]
> ?with(a = 3, f = {# * a}, 5.f) => 15 ; local variables and functions
: ?values
: ?calls
: ?operators
: ?blocks
: ?variables
: ?alphabet
: ?builtins
"#);
    topics.insert("?values", "Values", r#"
Number: an integer of any size. Other bases are written as `16_ff` (a base from 2 to 36, then the digits) or with the prefixes `0x`, `0o`, `0b`.
Boolean: `true` or `false`.
Character: in single quotes. It may hold several letters, like `'ch'`, for alphabets where they count as one.
String: in double quotes. A string is a type of its own, not a stream of characters; ?chars and ?string convert between the two.
Stream: items of any types in square brackets, or the result of a function. Streams are evaluated lazily and may be infinite.
Characters and strings accept the escapes `\n`, `\r`, `\t`, `\\`, `\'` and `\"`.
* The type of a value can be tested by ?isnum, ?isbool, ?ischar, ?isstring and ?isstream.
> 16_ff + 0b101 => 260
> [1, 'a', "abc", true, [2, 3]] : 10 => [1, 'a', "abc", true, [2, 3]]
> "abc".?chars => ['a', 'b', 'c']
> ['a', 'b', 'c'].?string => "abc"
> ?seq => [1, 2, 3, 4, 5, ...]
: ?alphabet
: ?operators
: ~
"#);
    topics.insert("?calls", "Calls, chaining and parts", r#"
A function may take a source, written before a dot, and arguments in parentheses. Which of them it expects is listed as the usage in its documentation.
The result of one call can be the source of the next: `a.f.g(x)` applies `g(x)` to the result of `a.f`.
`source[index]` is the item of a stream or the character of a string at `index`, counting from 1. See ?part for streams of indices and for nested parts.
`stream:func(args)` applies `func(args)` to every item of `stream` as its source.
`func@stream` calls `func` with the items of `stream` as its arguments.
* Names are case-insensitive, spaces are ignored and `;` starts a comment.
= func(arguments)
= source.func(arguments)
= source[index, ...]
= stream:func(arguments)
= func@stream
> ?range(3) => [1, 2, 3] ; arguments only
> [4, 5, 6].?len => 3 ; source only
> ?seq.?first(3) => [1, 2, 3] ; both
> "Hello, world!".?lcase.?chars.?select(?isalpha).?ddup : 10 => ['h', 'e', 'l', 'o', 'w', 'r', 'd']
> "abcde"[2..4] => "bcd"
> [[1, 2], [3, 4]][2, 1] => 3
> ["ab", "cde"]:?len => [2, 3]
> ?range@[2, 5] => [2, 3, 4, 5]
: ?blocks
: ?operators
: part
: map
"#);
    topics.insert("?operators", "Operators and precedence", r#"
From the tightest binding to the loosest:
`.` `:` `[...]` `@` (chaining, map, parts, arguments from a stream),
`^` (power),
`*` `/` (multiplication, integer division),
`%` (remainder),
`+` `-` (addition, subtraction), also prefix `-` and `!` (negation, logical NOT),
`..` (range),
`~` (concatenation),
`==` `<>` `<` `>` `<=` `>=` (comparison) and `<<` `>>` `<<=` `>>=` (lexical comparison),
`&` (logical AND),
`|` (logical OR),
`=` (assignment).
Arithmetic applies itemwise to streams and shifts characters and strings in the alphabet.
* A comparison can be chained with itself, but not with a different one.
! A prefix `-` or `!` needs parentheses after `+`, `-`, `%`, `*`, `/` and `^`.
! `=` only assigns variables; use `==` to compare.
> 1 + 2 * 3^2 => 19
> 1 + 2.{# * 10} => 21 ; chaining binds first
> 7 % 4 * 2 => 7 ; evaluated as `7 % (4 * 2)`
> 2^3^2 => 64 ; evaluated as `(2^3)^2`
> -2^2 => -4
> 2 * -3 => !cannot appear here ; write `2 * (-3)`
> -2..2 => [-2, -1, 0, 1, 2]
> 1..3 ~ 5..6 => [1, 2, 3, 5, 6]
> 1 < 2 < 3 => true
> 1 < 2 <= 3 => !cannot mix with '<', please rewrite as separate conditions
> 1 < 2 & 2 <= 3 => true
: +
: -
: *
: /
: %
: ^
: ..
: ~
: ==
: <>
: <
: <<
: &
: |
: !
: =
"#);
    topics.insert("?blocks", "Blocks and functions", r#"
A block is an expression in braces: a function written in place. Inside it, `#` stands for the source, `#1`, `#2`, ... for the arguments and `##` for all the arguments as a stream.
A block is used like any function name: with a source, with arguments, after `:` or as an argument of another function.
For any function, `func{body}` is a shorthand for `func({body})`. A function given as an argument can also be a bare name, as in `?select(?isodd)`.
Whether a function receives its input as `#` or as `#1`, `#2`, ... depends on what calls it: ?select passes each item as the source, ?fold passes two values as arguments.
A block `{...}` is evaluated where it stands: it sees the local variables and the alphabet around it. A block `${...}` is closed: it sees no local variables and uses the default alphabet, wherever it appears.
* `${...}` reaches a builtin hidden by a local variable of the same name.
* Closed blocks also appear when a function or a lazy value from elsewhere is described, such as a global variable or a history item.
= {body}
= ${body}
= func{body}
> 3.{# * 2} => 6
> 3.{# * #1 + #2}(4, 5) => 17
> {#1 + #2}(3, 4) => 7
> {##}(3, 4) => [3, 4]
> ?range(10).?select{# % 3 == 1} => [1, 4, 7, 10]
> ?range(10).?select(?isodd) => [1, 3, 5, 7, 9]
> ?range(5).?fold{#1 * #2} => [1, 2, 6, 24, 120]
> ?range(5).?fold(?times) => [1, 2, 6, 24, 120]
> ?with(len = {0}, [1, 2].len) => 0
> ?with(len = {0}, ${[1, 2].?len}) => 2
: ?variables
: ?calls
: with
: map
"#);
    topics.insert("?variables", "Variables and history", r#"
Local variables: `?with(name = value, ..., expr)` defines names for the evaluation of `expr`. A value in braces defines a function.
The assignments are processed from left to right, and a function sees the variables and the alphabet in effect where it was defined.
Global variables: names starting with `$`, assigned by `$name = value` as the whole input. They stay defined until removed by `clear($name)`.
A global function, `$name = {body}`, is a closed block: it sees no local variables and uses the default alphabet.
Other global variables and history items used in an assignment are filled in at that moment, so changing them later has no effect on it.
History: `%` is the last result and `%1`, `%2`, ... the result with that number. `$#` is the number of the current input, counting assignments as well.
= ?with(name = value, ..., expr)
= $name = value
= $name1 = $name2 = value
= clear($name, ...)
> ?with(a = 3, b = a + 1, a * b) => 12
> ?with(sq = {#^2}, ?range(4):sq) => [1, 4, 9, 16]
> ?with(add = {#1 + #2}, add(3, 4)) => 7
> ?with(a = 1, f = {# + a}, a = 10, 5.f) => 6 ; `f` keeps the `a` it was defined with
: ?blocks
: ?alphabet
: with
: =
"#);
    topics.insert("?alphabet", "Alphabets", r#"
Characters are calculated with by their position in the current alphabet, by default the English letters `'a'` to `'z'`.
Arithmetic wraps around at the end of the alphabet and keeps the letter case. In a string, characters outside the alphabet are left unchanged.
`?alpha(alphabet, expr)` evaluates `expr` under a different alphabet, given as a string or a stream of characters.
The alphabet determines arithmetic on characters and strings, ?ord and ?chr, character ranges, and the order used by ?sort and by the lexical comparisons `<<`, `>>`, `<<=`, `>>=`.
* Letters are matched regardless of case.
> 'z' + 1 => 'a'
> "Hello, world!" + 1 => "Ifmmp, xpsme!"
> "Wzmaj xdmms" + "key".?repeat => "Hello world"
> 'a'..'e' => ['a', 'b', 'c', 'd', 'e']
> ?alpha("zyxwvutsrqponmlkjihgfedcba", "hello".?chars:?ord) => [19, 22, 15, 15, 12]
> ?alpha(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'ch', 'i'], 'h' + 1) => 'ch'
> ["pear", "Apple", "fig"].?sort => ["Apple", "fig", "pear"]
: ?values
: alpha
: +
: <<
"#);
    topics.insert("?builtins", "Builtin functions by topic", r#"
Creating streams: ?seq, ?range, ?repeat, ?nest, ?self, ?primes, ?pi, ?rnd, ?perm.
Selecting items: ?part, ?first, ?last, ?skip, ?droplast, ?stride, ?select, ?while, ?rising, ?ddup, ?drep, ?remove, ?replace, ?reorder.
Rearranging and combining: ?rev, ?sort, ?flatten, ?transpose, ?zip, ?weave, ?unweave, ?riffle, ?join, ?prepend, ?append, ?padleft, ?padright.
Splitting and grouping: ?groups, ?partition, ?windows, ?split, ?consec, ?collect, ?runs, ?unroll, ?enum, ?counts.
Applying functions: ?foreach, ?fold, ?reduce, ?if, ?with.
Summarizing: ?len, ?total, ?min, ?max, ?dot.
Searching and testing: ?index, ?indices, ?contains, ?isin, ?countif, ?all, ?some, ?isempty.
Strings: ?chars, ?string, ?cat, ?ucase, ?lcase, ?subst, ?spliton, ?startswith, ?endswith.
Characters: ?alpha, ?ord, ?chr, ?uniord, ?unichr, ?isalpha, ?isascii, ?isdigit, ?iswhite, ?isupper, ?islower, ?isnumeric.
Numbers and digits: ?numstr, ?strnum, ?numdig, ?dignum, ?digstr, ?strdig, ?digits.
Arithmetic: ?abs, ?sign, ?sqrt, ?floor, ?ceiling, ?clamp, ?iswithin, ?isodd, ?iseven, ?greedy.
Number theory: ?isprime, ?factor, ?divisors, ?gcd, ?egcd, ?lcm, ?mod, ?modinv, ?modpow, ?crt.
Combinatorics: ?factorial, ?binom, ?multi, ?rmulti.
Types: ?isnum, ?isbool, ?ischar, ?isstring, ?isstream.
Operators as functions: ?plus, ?times, ?equal, ?and, ?or, ?not, ?xor.
: ?operators
: ?calls
"#);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::docs::{self, RefStringItem};
    use crate::symbols::tests::*;

    #[test]
    fn test_topics() -> Result<(), DocError> {
        let mut res = Ok(());
        for (name, docs) in &TOPICS.0 {
            if docs.desc.is_empty() {
                println!("{name:?}: empty description");
                res = Err(DocError);
            }
            for (line, _) in &docs.desc {
                res = res.and(check_refs(line, "?"));
            }
            for ex in &docs.examples {
                res = res.and(test_example(ex, "?"));
                res = res.and(check_refs(ex.input, "?"));
                if let Some(comment) = ex.comment {
                    res = res.and(check_refs(comment, "?"));
                }
            }
            for see in &docs.see {
                res = res.and(check_ref(see, name));
            }
        }
        res
    }

    #[test]
    fn test_builtins_complete() {
        let listed = TOPICS.0["?builtins"]
            .desc
            .iter()
            .flat_map(|(line, _)| docs::parse_line(line, "?"))
            .flat_map(|part| part.content)
            .filter_map(|item| match item {
                RefStringItem::Ref(sym) => Some(sym),
                _ => None,
            })
            .collect::<Vec<_>>();
        let missing = documented_names()
            .into_iter()
            .filter(|sym| !listed.contains(sym))
            .collect::<Vec<_>>();
        assert!(missing.is_empty(), "not listed in ??builtins: {missing:?}");
    }
}
