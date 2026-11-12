use streamlang as stream;
use stream::session::*;
use stream::base::*;
use std::cell::RefCell;

use wasm_bindgen::prelude::*;
use js_sys::{Object, Reflect, Array};

thread_local! {
    static SESS: RefCell<Session> = RefCell::new(Session::new());
}

#[wasm_bindgen]
pub fn process(input: String) -> Result<JsValue, JsValue> {
    console_error_panic_hook::set_once();
    let obj = Object::new();
    match stream::parse(&input) {
        Ok(expr) => SESS.with(|sess| -> Result<(), JsValue> {
            match sess.borrow_mut().process(expr) {
                Ok(SessionUpdate::History(index, item)) => {
                    Reflect::set(&obj, &"type".into(), &"item".into())?;
                    Reflect::set(&obj, &"history".into(), &index.into())?;
                    Reflect::set(&obj, &"value".into(), &format!("{item:.80}").into())?;
                    Reflect::set(&obj, &"itemType".into(), &item.type_str().into())?;
                },
                Ok(SessionUpdate::Globals(list)) => {
                    Reflect::set(&obj, &"type".into(), &"globals".into())?;
                    Reflect::set(&obj, &"names".into(), &format!("{}", list.len()).into())?;
                },
                Err(err) => {
                    Reflect::set(&obj, &"type".into(), &"error".into())?;
                    Reflect::set(&obj, &"text".into(), &format!("{err}").into())?;
                    Reflect::set(
                        &obj,
                        &"trace".into(),
                        &err.backtrace()
                            .iter()
                            .rev()
                            .map(|expr| JsValue::from(&expr.describe()))
                            .collect::<Array>()
                            .into(),
                    )?;
                },
            }
            Ok(())
        })?,
        Err(err) => {
            Reflect::set(&obj, &"type".into(), &"error".into())?;
            Reflect::set(&obj, &"text".into(), &format!("{err}").into())?;
            if let Some(range) = err.range_within(&input) {
                Reflect::set(
                    &obj,
                    &"range".into(),
                    &[range.start, range.end]
                        .into_iter()
                        .map(JsValue::from)
                        .collect::<Array>()
                        .into(),
                )?;
            }
        },
    }
    Ok(obj.into())
}

/// Drops the session: the history, the global variables and the environment. Also clears a
/// pending stop request.
///
/// The session belongs to the thread that calls `process()`, so this must be called on the same
/// thread, between requests; it cannot cut a running evaluation short. Free any `BoxedIterator`
/// first: one that is still alive stays usable, but keeps its stream (and anything it cached)
/// alive.
#[wasm_bindgen]
pub fn reset() {
    stream::base::stop::reset_stop();
    SESS.with(|sess| *sess.borrow_mut() = Session::new());
}

/// Requests the running evaluation to stop. The request stays in effect until the next
/// `process()`, `get_iter()` or `reset()` call clears it, so a stop issued between two `next()`
/// calls still interrupts the next one. The page must not issue a request while another one is outstanding,
/// otherwise the queued request would clear a stop meant for it.
#[wasm_bindgen]
pub fn stop() {
    stream::base::stop::send_stop()
}

#[wasm_bindgen]
pub fn validate(input: String) -> Result<JsValue, JsValue> {
    let obj = Object::new();
    match stream::parse(&input) {
        Ok(_) => {
            Reflect::set(&obj, &"valid".into(), &JsValue::TRUE)?;
        },
        Err(err) => {
            Reflect::set(&obj, &"valid".into(), &JsValue::FALSE)?;
            if let Some(range) = err.range_within(&input) {
                Reflect::set(
                    &obj,
                    &"range".into(),
                    &[range.start, range.end]
                        .into_iter()
                        .map(JsValue::from)
                        .collect::<Array>()
                        .into(),
                )?;
            }
        },
    }
    Ok(obj.into())
}

/// A handle to a running iteration over a stream from the session history.
///
/// The handle owns Rust memory and keeps the stream (and anything it cached) alive. The page
/// should call `.free()` on it, or hold it with `using`, once the listing is done or abandoned;
/// otherwise it is released only when the JS garbage collector gets to the wrapper.
#[wasm_bindgen]
pub struct BoxedIterator {
    iter: Box<dyn SIterator<Item>>,
    done: bool,
}

#[wasm_bindgen]
impl BoxedIterator {
    /// Returns `{done: false, value}` with an item object for an item, `{done: false, value}`
    /// with an error object if the iteration failed, or `{done: true}` at the end of the stream.
    /// After an error or the end, the iterator is finished and further calls return
    /// `{done: true}`.
    #[allow(clippy::should_implement_trait)] // the JS iterator protocol requires this name
    pub fn next(&mut self) -> Result<JsValue, JsValue> {
        if self.done {
            let obj = Object::new();
            Reflect::set(&obj, &"done".into(), &JsValue::TRUE)?;
            return Ok(obj.into());
        }
        let value = match self.iter.next() {
            Ok(Some(item)) => {
                let obj = Object::new();
                Reflect::set(&obj, &"type".into(), &"item".into())?;
                Reflect::set(&obj, &"value".into(), &format!("{item:.80}").into())?;
                Reflect::set(&obj, &"itemType".into(), &item.type_str().into())?;
                obj
            },
            Ok(None) => {
                self.done = true;
                let obj = Object::new();
                Reflect::set(&obj, &"done".into(), &JsValue::TRUE)?;
                return Ok(obj.into());
            },
            Err(err) => {
                self.done = true;
                let obj = Object::new();
                Reflect::set(&obj, &"type".into(), &"error".into())?;
                Reflect::set(&obj, &"text".into(), &format!("{err}").into())?;
                Reflect::set(
                    &obj,
                    &"trace".into(),
                    &err.backtrace()
                        .iter()
                        .rev()
                        .map(|expr| JsValue::from(&expr.describe()))
                        .collect::<Array>()
                        .into(),
                )?;
                obj
            },
        };
        let obj = Object::new();
        Reflect::set(&obj, &"value".into(), &value.into())?;
        Reflect::set(&obj, &"done".into(), &JsValue::FALSE)?;
        Ok(obj.into())
    }
}

#[wasm_bindgen]
pub fn get_iter(index: usize) -> Result<BoxedIterator, JsValue> {
    SESS.with(|sess| {
        let sess = sess.borrow();
        if index == 0 {
            return Err("invalid index".into());
        }
        let Some(item) = sess.history().get(index - 1) else {
            return Err(format!("no such history index {index} len {}", sess.history().len()).into());
        };
        let Item::Stream(stm) = item else {
            return Err("not a stream".into());
        };
        stream::base::stop::reset_stop();
        Ok(BoxedIterator { iter: stm.iter(), done: false })
    })
}

fn object(fields: &[(&str, JsValue)]) -> Result<JsValue, JsValue> {
    let obj = Object::new();
    for (key, value) in fields {
        Reflect::set(&obj, &(*key).into(), value)?;
    }
    Ok(obj.into())
}

/// A line of documentation in the form described at `help()`. A reference to `page` itself is
/// left a plain string.
fn help_line(line: &str, sym: &str, page: &stream::docs::DocRecord) -> Result<JsValue, JsValue> {
    use stream::docs::{LinePart, RefStringItem};
    let mut parts = Array::new();
    for LinePart { content, is_code } in stream::docs::parse_line(line, sym) {
        let mut items = Array::new();
        for item in content {
            items.extend([match item {
                RefStringItem::Ref(name) if stream::find_docs(name).is_some_and(|docs| !std::ptr::eq(docs, page)) => {
                    object(&[("ref", name.into()), ("text", name.into())])?
                },
                RefStringItem::Base(text) | RefStringItem::Ref(text) => text.into(),
            }]);
        }
        parts.extend([object(&[("code", is_code.into()), ("items", items.into())])?]);
    }
    Ok(parts.into())
}

/// Returns the documentation of a symbol or a help page on the language: the quick guide under
/// an empty `name`, its sections under the names it refers to, which start with `?`.
///
/// The result is `{found: true, title, synonyms, usage, desc, examples, see}`. `usage` is an
/// array of lines (see below), `desc` of `{kind, line}` with a kind of `"base"`, `"tip"` or
/// `"warn"`, `examples` of `{input, comment, output, failed}` where `input` and the optional
/// `comment` are lines and `output` is the result or, if `failed`, the error message. `see` is
/// an array of references.
///
/// A line is an array of parts `{code, items}`, where `code` tells whether the part is program
/// text and each item is either a string or a reference. A reference is `{ref, text}`: the
/// name of another page, to be passed to this function, and the text to show for it.
///
/// For an unknown name the result is `{found: false, topic}`; `topic` tells that a help page of
/// that name exists, which is to be asked for as `?name`.
///
/// This does not touch the session, so it may be called on any thread at any time.
#[wasm_bindgen]
pub fn help(name: String) -> Result<JsValue, JsValue> {
    use stream::docs::DescType;
    let Some(docs) = stream::find_docs(&name) else {
        let topic = stream::find_docs(&format!("?{name}")).is_some();
        return object(&[("found", false.into()), ("topic", topic.into())]);
    };
    // A help page has a title of its own, and a lone `?` in it stands for itself.
    let sym = if docs.title.is_some() { "?" } else { name.as_str() };
    let line = |text: &str| help_line(text, sym, docs);

    let synonyms = docs
        .symbols
        .iter()
        .filter(|s| **s != sym)
        .map(|s| JsValue::from(*s))
        .collect::<Array>();
    let usage = docs
        .usage
        .iter()
        .map(|text| line(text))
        .collect::<Result<Array, _>>()?;
    let desc = docs
        .desc
        .iter()
        .map(|(text, kind)| {
            let kind = match kind {
                DescType::Base => "base",
                DescType::Tip => "tip",
                DescType::Warn => "warn",
            };
            object(&[("kind", kind.into()), ("line", line(text)?)])
        })
        .collect::<Result<Array, _>>()?;
    let examples = docs
        .examples
        .iter()
        .map(|example| {
            object(&[
                ("input", line(example.input)?),
                ("comment", example.comment.map(line).transpose()?.into()),
                ("output", example.output.unwrap_or_else(|err| err).into()),
                ("failed", example.output.is_err().into()),
            ])
        })
        .collect::<Result<Array, _>>()?;
    let see = docs
        .see
        .iter()
        .map(|name| {
            let text = stream::find_docs(name).and_then(|docs| docs.title).unwrap_or(name);
            object(&[("ref", (*name).into()), ("text", text.into())])
        })
        .collect::<Result<Array, _>>()?;

    object(&[
        ("found", true.into()),
        ("title", docs.title.unwrap_or(sym).into()),
        ("synonyms", synonyms.into()),
        ("usage", usage.into()),
        ("desc", desc.into()),
        ("examples", examples.into()),
        ("see", see.into()),
    ])
}
