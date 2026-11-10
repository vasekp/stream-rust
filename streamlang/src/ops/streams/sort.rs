use crate::base::*;

use std::cmp::Ordering;

fn eval_sort(node: &Node, env: &Env) -> SResult<Item> {
    let source = node.source_checked()?.eval(env)?.to_stream()?;
    let (func, vals, keys) = match &node.args[..] {
        [] => (None, source.listout()?, None),
        [expr] => {
            let func = Function::new(expr.as_func()?, env);
            let (keys, vals): (Vec<_>, Vec<_>) = source
                .listout_with(|item| func.call_source(item.clone()).map(|key| (key, item)))?
                .into_iter()
                .unzip();
            (Some(func), vals, Some(keys))
        },
        _ => return Err(StreamError::usage(&node.head)),
    };
    let data = SortData { vals, keys, alpha: Rc::clone(&env.alpha) };
    let mut heap = SortHeap::new(data)?;
    if heap.len() <= CACHE_LEN {
        let mut sorted = Vec::with_capacity(heap.len());
        while let Some(item) = heap.pop()? {
            sorted.push(item);
        }
        Ok(Item::new_stream(List::from(sorted)))
    } else {
        Ok(Item::new_stream(Sorted { head: node.head.clone(), source, func, env: env.clone(), heap }))
    }
}

struct SortData {
    vals: Vec<Item>,
    keys: Option<Vec<Item>>,
    alpha: Rc<Alphabet>,
}

impl SortData {
    fn key(&self, ix: usize) -> &Item {
        self.keys.as_ref().map_or(&self.vals[ix], |keys| &keys[ix])
    }

    fn less(&self, a: usize, b: usize) -> SResult<bool> {
        Ok(self.key(a).lex_cmp(self.key(b), &self.alpha)? == Ordering::Less)
    }
}

#[derive(Clone)]
struct SortHeap {
    data: Rc<SortData>,
    indices: Vec<usize>,
}

impl SortHeap {
    pub fn new(data: SortData) -> SResult<Self> {
        let size = data.vals.len();
        let mut heap = Self { data: Rc::new(data), indices: (0..size).collect::<Vec<_>>() };
        for pos in (0..size / 2).rev() {
            heap.sift_down(pos)?;
        }
        Ok(heap)
    }

    pub fn len(&self) -> usize {
        self.indices.len()
    }

    pub fn clear(&mut self) {
        self.indices.clear()
    }

    fn sift_down(&mut self, mut pos: usize) -> SResult<()> {
        let len = self.indices.len();
        loop {
            check_stop!();
            let left = 2 * pos + 1;
            if left >= len {
                return Ok(());
            }
            let right = left + 1;
            let child =
                if right < len && self.data.less(self.indices[right], self.indices[left])? { right } else { left };
            if !self.data.less(self.indices[child], self.indices[pos])? {
                return Ok(());
            }
            self.indices.swap(pos, child);
            pos = child;
        }
    }

    fn pop_index(&mut self) -> SResult<Option<usize>> {
        let Some(last) = self.indices.pop() else {
            return Ok(None);
        };
        if self.indices.is_empty() {
            return Ok(Some(last));
        }
        let top = std::mem::replace(&mut self.indices[0], last);
        self.sift_down(0)?; // on error the heap is left inconsistent and must not be used again
        Ok(Some(top))
    }

    pub fn pop(&mut self) -> SResult<Option<Item>> {
        Ok(self.pop_index()?.map(|ix| self.data.vals[ix].clone()))
    }

    pub fn skip(&mut self, count: usize) -> SResult<()> {
        for _ in 0..count {
            self.pop_index()?;
        }
        Ok(())
    }
}

struct Sorted {
    head: Head,
    source: Rc<dyn Stream>,
    /// Only kept for the description: the keys are computed up front.
    func: Option<Function>,
    /// Needed for the ordering, not only for `func`.
    env: Env,
    heap: SortHeap,
}

impl Describe for Sorted {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_args(&self.func)
            .finish(prec)
    }
}

impl Stream for Sorted {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        SortedIter { heap: self.heap.clone(), node: self }.wrap()
    }

    fn len(&self) -> Length {
        Length::from(self.heap.len())
    }
}

struct SortedIter {
    node: Rc<Sorted>,
    heap: SortHeap,
}

impl PreIterator for SortedIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        self.heap.pop()
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        match usize::try_from(n) {
            Ok(n) if n <= self.heap.len() => {
                self.heap.skip(n)?;
                Ok(None)
            },
            _ => {
                let remain = n - self.heap.len();
                self.heap.clear();
                Ok(Some(remain))
            },
        }
    }

    fn origin(&self) -> &Rc<Sorted> {
        &self.node
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("sort", eval_sort, r#"
Sorts the input `stream`. Arrays of numbers, characters, strings, or streams can be sorted.
Strings and streams are sorted in lexicographic order.
If `func` is provided, compares `item`s using the value of `item.func`.
The relative order of items whose keys compare equal is unspecified.
= stream.?
= stream.?{func}
> [5, 2, 9, 3, 5].? => [2, 3, 5, 5, 9]
> [5, 2, 9, 3, 5].?{-#} => [9, 5, 5, 3, 2] ; reverse sort
> ['b', 'a', 'c'].? => ['a', 'b', 'c']
> ["one", "two", "three"].? => ["one", "three", "two"]
> ["one", "two", "three", "four"].?(?length) => ["one", "two", "four", "three"]
> [[1,2], [], [1]].? : 6 => [[], [1], [1, 2]]
: <<
: >>
"#);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_sort() {
        use super::*;

        test_eval!("[5,2,9,3,5].sort" => "[2, 3, 5, 5, 9]");
        test_eval!("[].sort" => "[]");
        test_eval!("[1].sort" => "[1]");
        test_eval!("\"bac\".sort" => err);
        test_eval!("['b','a','c'].sort" => "['a', 'b', 'c']");
        test_eval!("[\"abc\", \"def\", \"AdS\"].sort" => "[\"abc\", \"AdS\", \"def\"]");
        test_eval!("alpha(\"aáb\", [\"a\", \"B\", \"Á\"].sort)" => "[\"a\", \"Á\", \"B\"]");
        test_eval!("[1,'a'].sort" => err);
        test_eval!("[[1,2],[1,'a']].sort" => err);

        test_eval!("[5,2,9,3,5].sort{-#}" => "[9, 5, 5, 3, 2]");
        test_eval!("[\"one\", \"two\", \"three\"].sort(rev)" => "[\"three\", \"one\", \"two\"]");

        test_eval!("[1,2,3].perm:sort.all{#==[1,2,3]}" => "true");
        test_eval!("[1,2,3,4].perm:sort.all{#==[1,2,3,4]}" => "true");
        test_eval!("[1,2,3,4,5].perm:sort.all{#==[1,2,3,4,5]}" => "true");

        // Inputs beyond CACHE_LEN are sorted lazily.
        test_eval!("range(1000).sort == range(1000)" => "true");
        test_eval!("range(1000).rev.sort == range(1000)" => "true");
        test_eval!("(range(500).rev~range(500)).sort == range(500):{#.repeat(2)}.flatten" => "true");
        test_eval!("range(1000):{(#*7919)%1000}.sort == range(0,999)" => "true");
        test_eval!("range(1000).rev.sort{-#} == range(1000).rev" => "true");
        test_eval!("(range(200):{-#}~range(200)).sort{#.abs}:abs == range(200):{#.repeat(2)}.flatten" => "true");
        test_eval!("1.repeat(1000).sort" => "[1, 1, 1, 1, 1, ...]");
        test_eval!("range(10^5).rev.sort[1]" => "1");
        test_eval!("range(10^5).sort[10^5]" => "100000");
        test_eval!("range(10^5).sort[10^5+1]" => err);
        test_eval!("1.repeat(10^5).sort[10^5]" => "1");
        test_eval!("alpha(\"cba\", ['a','b','c'].repeat(50).sort)" => "['c', 'c', 'c', 'c', 'c', ...]");
        // Type mismatch surfaces during heapify, i.e. at evaluation time.
        test_eval!("(range(200)~['a']).sort" => err);
        test_eval!("(range(200)~[[1,'a']]).sort" => err);
        test_eval!("(range(200)~[1]).sort{if(#==1, 'a', #)}" => err);
        // A comparison that fails only for one particular pair surfaces lazily if the pair is
        // only compared after heapify.
        test_eval!("alpha(\"abz\", ([\"zb\", \"zx\"]~[\"bb\"].repeat(200)).sort)[1]" => "\"bb\"");
        test_eval!("alpha(\"abz\", ([\"zb\", \"zx\"]~[\"bb\"].repeat(200)).sort)[202]" => err);
        test_eval!("alpha(\"abz\", ([\"zb\", \"zx\"]~[\"bb\"].repeat(200)).sort).last" => err);

        test_len!("[].sort" => 0);
        test_len!("[3,1,2].sort" => 3);
        test_len!("range(1000).sort" => 1000);
        test_len!("range(1000).rev.sort" => 1000);
        test_len!("range(1000).sort{-#}" => 1000);
        test_advance("range(1000).sort");
        test_advance("range(1000).rev.sort");
        test_advance("(range(500).rev~range(500)).sort");
        test_advance("range(1000):{(#*7919)%1000}.sort{-#}");
        test_advance("1.repeat(1000).sort");

        test_describe!("[3,1,2].sort" => "[1, 2, 3]");
        test_describe!("[3,1,2].sort{-#}" => "[3, 2, 1]");
        test_describe!("range(1000).sort" => "range(1000).sort");
        test_describe!("range(1000).sort{-#}" => "range(1000).sort({-#})");
        test_describe!("alpha(\"cba\", ['a','b','c'].repeat(50).sort)" => "alpha(['c', 'b', 'a'], ['a', 'b', 'c'].repeat(50).sort)");
    }
}
