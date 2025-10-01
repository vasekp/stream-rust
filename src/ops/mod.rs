mod lang;
mod streams;
mod selfref;
mod env;
mod strings;
mod cond;
mod conv;
mod ana;
mod rnd;
mod combi;

#[cfg(test)]
mod testutils;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    lang::init(keywords);
    streams::init(keywords);
    selfref::init(keywords);
    env::init(keywords);
    strings::init(keywords);
    cond::init(keywords);
    conv::init(keywords);
    ana::init(keywords);
    rnd::init(keywords);
    combi::init(keywords);

    #[cfg(test)]
    testutils::init(keywords);
}

#[cfg(test)]
#[test]
fn misc_tests() {
    use crate::base::*;

    // Double factorial
    test_eval!("seq:{range(#,1,-2).fold{#*#1}(1).last}" : 10 => "[1, 2, 3, 8, 15, 48, 105, 384, 945, 3840, ...]");
    // Fibonacci
    test_eval!("nest{#1+#2}(1,1)" => "[2, 3, 5, 8, 13, ...]");
    test_eval!("nest({#1~#2}(1, 1)):len" => "[2, 3, 5, 8, 13, ...]");
    // Von Neumann numerals
    test_eval!("[].nest{#~[#]}[4]" : 15 => "[[], [[]], [[], [[]]], [[], [[]], [[], [[]]]]]");
    // Binomial coefficients
    test_eval!("[1].nest{(0~#)+(#~0)}[4]" => "[1, 4, 6, 4, 1]");
    test_eval!("[1].nest{(0~#~0).windows(2, plus)}" : 18 => "[[1, 1], [1, 2, 1], [1, 3, 3, 1], [1, 4, 6, 4, 1], ...]");
    // 10-adic automorphic number
    test_eval!("5.nest{#^2%10^50}.windows(2).select{#[1]==#[2]}.first.first" => "57423423230896109004106619977392256259918212890625");
    // Collatz
    test_eval!("9.nest{if(#.isodd, 3*#+1, #/2)}.while{#<>1}" : 50 => "[28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2]");
    test_eval!("seq:{#.nest{if(#.isodd, 3*#+1, #/2)}.pos(1)}" : 10 => "[3, 1, 7, 2, 5, 8, 16, 3, 19, 6, ...]");

    // Hamming weights
    test_eval!("self{([0,1]~#.skip(2)).riffle(1+#)}" : 33 => "[0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 1, ...]");
    test_eval!("seq:numdig(2):count(1)" : 32 => "[1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 1, ...]");
    // Trailing zeroes
    test_eval!("self{0.repeat.riffle(#+1)}" : 32 => "[0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, ...]");
    test_eval!("seq:{(#~#.nest{#/2}).while(iseven).len}" : 32 => "[0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, ...]");
    test_eval!("seq:numstr(2):rev:{#.chars.while{#=='0'}.len}" : 32 => "[0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, ...]");
    // Binary length
    test_eval!("self{(1~(#+1)).riffle(#+1)}" : 32 => "[1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, ...]");
    test_eval!("seq:{(#~#.nest{#/2}).while{#>0}.len}" : 32 => "[1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, ...]");
    test_eval!("seq:numdig(2):len" : 32 => "[1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, ...]");
    // Thue-Morse
    test_eval!("self{([0,1]~#.skip(2)).riffle(1-#)}" : 33 => "[0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, ...]");
    test_eval!("seq:numdig(2):count(1)%2" : 32 => "[1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, ...]");
    // Paperfolding sequence
    test_eval!("self{[0,1].repeat.riffle(#)}" : 33 => "[0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, ...]");
    // Hanoi towers
    test_eval!("self{[12,23,31].repeat.riffle([13,32,21].repeat.riffle(#))}" : 16 => "[12, 13, 23, 12, 31, 32, 12, 13, 23, 21, 31, 23, 12, 13, 23, 12, ...]");

    // Triangular coordinates
    test_eval!("seq:{with(a=#,range(1,#):{[a,#]})}.flatten(1)" : 18 => "[[1, 1], [2, 1], [2, 2], [3, 1], [3, 2], [3, 3], ...]");

    // π string
    test_eval!("\"0123456789\"[pi+1]" => "\"31415926535897932384...");
    // π using letters
    test_eval!("pi.while{#>0}:chr.cat" => "\"cadaeibfecehigicbchd...");
    // π zeroes
    test_eval!("pi.enum.select{#[1]==0}:last" => "[33, 51, 55, 66, 72, ...]");
}
