# Stream

Stream is a simple data processing language, designed with ciphers and puzzles in mind. It works
with lazy, possibly infinite streams, integers of any size, and characters and strings that can be
calculated with as letters of an alphabet.

```
> "Uryyb, jbeyq!" + 13
%1: "Hello, world!"
> "Stream".chars:ord
%2: [19, 20, 18, 5, 1, 13]
> "Wzmaj xdmms" + "key".repeat
%3: "Hello world"
> "Mississippi".lcase.counts
%4: [['m', 1], ['i', 4], ['s', 4], ['p', 2]]
> primes:{#^2}.first(5)
%5: [4, 9, 25, 49, 121]
> nest{#1+#2}(1,1)
%6: [2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6...
```

**[Try it in your browser](https://vasekp.github.io/stream-rust/)**, nothing to install.

This repository contains the base language library, its command-line REPL interface, and a
WebAssembly port for deployment in web applications.

Stream is the successor to [Stream](https://github.com/vasekp/stream). It is planned to become a
central component of [Puzzlehunt Assistant v3](https://github.com/vasekp/spa3), both under the
hood and as a directly accessible tool.

## Contents

- [Getting started](#getting-started)
- [Values](#values)
- [Expressions](#expressions)
- [Operators and precedence](#operators-and-precedence)
- [Blocks and functions](#blocks-and-functions)
- [Variables and scope](#variables-and-scope)
- [Alphabets](#alphabets)
- [Laziness and errors](#laziness-and-errors)
- [The command line](#the-command-line)
- [Finding the builtins](#finding-the-builtins)
- [Known limitations](#known-limitations)
- [Repository layout](#repository-layout)
- [Building the web package](#building-the-web-package)

## Getting started

```
cargo run --release -p stream-cli
```

This opens a prompt. Every input is one expression; its result is printed with a history number
(`%1`, `%2`, ...) by which later inputs can refer to it.

This README describes the syntax. The builtin functions and operators are documented in the
program itself: type `?` followed by a name.

```
> ?range
> ?+
```

## Values

| Type | Examples | Notes |
|---|---|---|
| Number | `42`, `-7`, `2^100` | Integers only, of unlimited size. |
| Boolean | `true`, `false` | |
| Character | `'a'`, `'7'`, `'ch'` | May hold more than one letter, for alphabets where a digraph counts as one. |
| String | `"Hello"`, `""` | |
| Stream | `[1, 2, 3]`, `[]`, `seq` | Items of any types, including other streams. May be infinite. |

Numbers can be written in other bases as `base_digits` with a base from 2 to 36 (`16_ff`, `2_101`),
or with the prefixes `0x`, `0o` and `0b`.

Characters and strings accept the escapes `\n`, `\r`, `\t`, `\\`, `\'` and `\"`.

A string is a type of its own, not a stream of characters. Many stream functions accept strings
too; `chars` and `string` convert between the two.

```
> "abc".chars
%1: ['a', 'b', 'c']
> ['a', 'b', 'c'].string
%2: "abc"
```

## Expressions

Names are case-insensitive and whitespace between tokens is ignored. A `;` starts a comment that
runs to the end of the input.

### Calls and chaining

A function may take a *source*, written before a dot, and *arguments*, written in parentheses.
Which of these a function expects is shown under "Usage" in its help.

```
> range(3)             ; arguments only
%1: [1, 2, 3]
> [4, 5, 6].len        ; source only
%2: 3
> seq.first(3)         ; both
%3: [1, 2, 3]
```

The result of one call can be the source of the next, so that an expression reads from left to
right:

```
> "Hello, world!".lcase.chars.select(isalpha).counts
%1: [['h', 1], ['e', 1], ['l', 3], ['o', 2], ['w', 1], ['r', 1], ['d', 1]]
```

### Parts

`source[index]` takes an item of a stream or a character of a string, counting from 1. A stream of
indices gives a stream of parts, and further indices reach into nested streams.

```
> seq[5]
%1: 5
> "abcde"[2..4]
%2: "bcd"
> [[1, 2], [3, 4]][2, 1]
%3: 3
```

### Applying to every item

`stream:func(args)` applies `func` to each item of the stream, with the item as its source. A
[block](#blocks-and-functions) can stand in place of the function.

```
> ["ab", "cde"]:len
%1: [2, 3]
> range(5):{#^2}
%2: [1, 4, 9, 16, 25]
```

### Arguments from a stream

`func@stream` calls `func` with the items of the stream as its arguments.

```
> range@[2, 5]
%1: [2, 3, 4, 5]
```

## Operators and precedence

From the tightest binding to the loosest:

| Operators | Meaning |
|---|---|
| `.` `:` `[...]` `@` | chaining, map, parts, arguments from a stream |
| `^` | power |
| `*` `/` | multiplication, integer division |
| `%` | remainder |
| `+` `-`, prefix `-` `!` | addition, subtraction, negation, logical NOT |
| `..` | range |
| `~` | concatenation |
| `==` `<>` `<` `>` `<=` `>=` `<<` `>>` `<<=` `>>=` | comparison; the doubled forms compare lexically |
| `&` | logical AND |
| `\|` | logical OR |
| `=` | assignment |

Parentheses group as usual. Each operator has its own help page (`?~`, `?<<`, ...).

Points worth knowing:

- **Chaining binds first.** `1+2.{#*10}` is `1 + 20`; write `(1+2).{#*10}` for the other meaning.
- **`%` sits between `+` and `*`.** `7 % 4 * 2` is `7 % 8`.
- **`^` groups from the left.** `2^3^2` is `(2^3)^2 = 64`.
- **Comparisons chain, but only with themselves.** `1 < 2 < 3` and `a == b == c` are fine;
  `1 < 2 <= 3` is an error and must be written as `1 < 2 & 2 <= 3`.
- **A prefix operator after a tighter binary operator needs parentheses.** `1..-1`, `a == -1` and
  `x & !y` work, but `2*-3`, `1+-2` and `2^-1` must be written as `2*(-3)` and so on.
  At the start of an expression, `-2^2` is `-(2^2)`.
- **`=` is not a comparison.** It only assigns [variables](#variables-and-scope); use `==` to
  compare.

Arithmetic applies itemwise to streams, to any depth, and combines streams with single values:

```
> [1, 2, 3] * 10 + [1, 1, 1]
%1: [11, 21, 31]
```

It also works on characters and strings, shifting letters within the
[alphabet](#alphabets):

```
> 'a' + 1
%1: 'b'
> "abc" + [1, 2, 3]
%2: "bdf"
> "IBM" - 1
%3: "HAL"
```

## Blocks and functions

An expression in braces is a *block*: a function written in place. Inside it,

| Symbol | Stands for |
|---|---|
| `#` | the source |
| `#1`, `#2`, ... | the arguments |
| `##` | all the arguments, as a stream |

A block is used like any named function:

```
> 3.{# * 2}
%1: 6
> 3.{# * #1 + #2}(4, 5)
%2: 17
> {#1 + #2}(3, 4)
%3: 7
```

Many builtins take a function as an argument. This can be a block or a bare function name:

```
> range(10).select({# % 3 == 1})
%1: [1, 4, 7, 10]
> range(10).select(isodd)
%2: [1, 3, 5, 7, 9]
> range(5).fold(plus)
%3: [1, 3, 6, 10, 15]
```

For any function, `op{func}` is a shorthand for `op({func})`:

```
> range(10).select{# % 3 == 1}
%1: [1, 4, 7, 10]
> range(5).fold{#1 * #2}
%2: [1, 2, 6, 24, 120]
```

Whether the function receives its input as `#` or as `#1`, `#2`, ... depends on the builtin and
follows from its help: `select` tests one item at a time and passes it as the source, `fold`
combines two values and passes them as arguments.

### Closed blocks

A block `{...}` is evaluated where it stands: it sees the local variables and the alphabet around
it. A block written as `${...}` is *closed*: it sees no local variables and uses the default
alphabet, wherever it appears. That makes it the way to reach a builtin that a local variable
hides:

```
> with(len = {0}, [1, 2].len)
%1: 0
> with(len = {0}, ${[1, 2].len})
%2: 2
```

Closed blocks are also what the program prints when it describes a function or a lazy value that
came from somewhere else, such as a global variable or the history (see `:desc` and `:vars`
[below](#the-command-line)).

## Variables and scope

### Local variables

`with(name = value, ..., expr)` defines names for the evaluation of `expr`. A value in braces
defines a function.

```
> with(a = 3, b = a + 1, a * b)
%1: 12
> with(sq = {#^2}, range(4):sq)
%2: [1, 4, 9, 16]
> with(add = {#1 + #2}, add(3, 4))
%3: 7
```

- The assignments are processed from left to right, so each can use the ones before it.
- A function sees the variables and the alphabet that were in effect where it was defined, not
  where it is used:

  ```
  > with(a = 1, f = {# + a}, a = 10, 5.f)
  %1: 6
  ```
- A local name may hide a builtin. Use `${...}` to reach the builtin again.

### Global variables

Names starting with `$` are global variables. They are assigned at the prompt and stay defined for
the rest of the session.

```
> $key = 3
$key = 3
> "abc" + $key
%1: "def"
> $double = {# * 2}
$double = ${#*2}
> range(3):$double
%2: [2, 4, 6]
> $a = $b = 5
$a = 5
$b = 5
> clear($a, $b)
```

- An assignment can only be the whole input, not a part of a larger expression.
- A global function is a closed block: it sees no local variables and uses the default alphabet.
- Other globals and history items that an assignment refers to are filled in at that moment.
  After `$f = {# + $key}`, changing `$key` does not change `$f`.

### History

| Symbol | Stands for |
|---|---|
| `%` | the last result |
| `%1`, `%2`, ... | the result with that number |
| `$#` | the number of the current input, starting from 1 and counting assignments as well |

```
> 6 * 7
%1: 42
> % + 1
%2: 43
> %1 * %2
%3: 1806
```

## Alphabets

Characters are calculated with by their position in the current alphabet, by default the English
letters `'a'` to `'z'`. Arithmetic wraps around at the end and keeps the letter case; characters
outside the alphabet pass through unchanged.

```
> 'z' + 1
%1: 'a'
> "Hello, world!" + 1
%2: "Ifmmp, xpsme!"
> "Stream".chars:ord
%3: [19, 20, 18, 5, 1, 13]
```

`alpha(alphabet, expr)` evaluates `expr` under a different alphabet, given as a string or a stream
of characters:

```
> alpha("zyxwvutsrqponmlkjihgfedcba", "hello".chars:ord)
%1: [19, 22, 15, 15, 12]
> alpha(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'ch', 'i'], 'h' + 1)
%2: 'ch'
```

The alphabet determines:

- arithmetic on characters and strings,
- `ord`, `chr` and related conversions,
- the order used by the lexical comparisons `<<`, `>>`, `<<=`, `>>=` and by `sort`,
- character ranges such as `'a'..'e'`.

Letters are matched regardless of case: `'a' <<= 'A'` and `'A' <<= 'a'` are both true, although
`'a' == 'A'` is not. See `?alpha` for details.

## Laziness and errors

Streams are evaluated lazily. An infinite stream is fine as long as only a finite part of it is
needed, and the prompt prints only as much of a result as fits on a line:

```
> seq:{#^2}
%1: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324...
> %[1000]
%2: 1000000
> seq.len
seq: stream is infinite
```

A function that needs all of an endless stream which does not announce itself as infinite, such as
`primes.select{# < 0}.first`, runs until interrupted with Ctrl-C.

An error inside a stream appears only when the failing item is reached. It is printed as `<!>` in
place of the item, followed by the message:

```
> seq:{10 / (3 - #)}
%1: [5, 10, <!>
10/(3-3): division by zero
```

Taking a part skips the items before it without evaluating them, so the same stream still has a
fifth item:

```
> seq:{10 / (3 - #)}[5]
%1: -5
```

## The command line

| Input | Effect |
|---|---|
| `?name` | Help for a builtin or operator: usage, description, examples, related entries. |
| `:list` or `:less` | Page through the whole last result, if it is a stream, one item per line. |
| `:desc` | Print the last result as an expression that evaluates to it. |
| `:vars` | List the global variables. |
| `:show $name` | Show one global variable. |
| `:bt` | After an error, show the chain of expressions that led to it. |
| `:trace on`, `:trace off` | Switch tracing of evaluation steps. |

Ctrl-C interrupts a running evaluation, Ctrl-D ends the session.

## Finding the builtins

There are about 150 builtin functions; this README does not list them. Each has a help page with
usage patterns, examples and a "See also" line leading to its relatives, so a few starting points
are enough:

| Area | Start from |
|---|---|
| Making streams | `?seq`, `?range`, `?nest`, `?repeat` |
| Reshaping streams | `?first`, `?select`, `?fold`, `?sort`, `?zip`, `?flatten` |
| Strings and characters | `?chars`, `?cat`, `?split`, `?subst`, `?ucase`, `?ord` |
| Conversions | `?numstr`, `?digits`, `?numdig` |
| Conditions | `?if`, `?while`, `?all`, `?isin`, `?isalpha` |
| Numbers | `?primes`, `?factor`, `?gcd`, `?mod`, `?binom`, `?perm` |
| Language | `?with`, `?alpha`, `?part`, `?map`, `?self` |

## Known limitations

- Very deeply nested input, in the range of tens of thousands of levels, overflows the stack and
  aborts the program.
- A single huge power such as `2^(10^9)` cannot be interrupted.
- Functions that need a whole stream at once (`func@stream`, `transpose` and others) have no
  ceiling on its length; given an enormous finite stream, they use memory until it runs out.

## Repository layout

| Directory | Contents |
|---|---|
| `streamlang` | The language as a Rust library: parser, evaluator, builtins and their documentation. |
| `stream-cli` | The command-line prompt described above. |
| `stream-wasm` | WebAssembly bindings for use in a web page. |

## Building the web package

`stream-wasm` needs a nightly toolchain with the `rust-src` component and the
`wasm32-unknown-unknown` target; `rust-toolchain.toml` in that directory selects them, and
`rustup` installs them on first use. With [wasm-pack](https://github.com/wasm-bindgen/wasm-pack)
installed:

```
cd stream-wasm
wasm-pack build
```

The package is written to `stream-wasm/pkg`. Add `--target web` or `--target nodejs` for use
without a bundler. The command has to be run inside `stream-wasm`, because the build settings in
its `.cargo/config.toml` apply only there.

The module is built with shared memory, so that a second worker can interrupt a running
evaluation. A page that loads it must therefore be served cross-origin isolated (with the
`Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp`
headers).

The package exports:

| Function | Purpose |
|---|---|
| `process(input)` | Evaluates one input in the session and returns the result, the assignment made, or the error. |
| `validate(input)` | Checks the syntax only, returning the position of an error. |
| `get_iter(index)` | Returns an iterator over the items of a stream in the history, for listing it lazily. |
| `stop()` | Asks the running evaluation to stop. |
| `reset()` | Drops the session, with its history and global variables, and starts an empty one. |
| `help(name)` | Returns the documentation of a builtin or an operator, or a help page (the quick guide for an empty name), as structured data for display. |
