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

/// Requests the running evaluation to stop. The request stays in effect until the next
/// `process()` or `get_iter()` call clears it, so a stop issued between two `next()` calls still
/// interrupts the next one. The page must not issue a request while another one is outstanding,
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
    /// Returns `{done: true}` at the end of the stream, `{done: false, value}` for an item, or
    /// `{done: true, value}` with an error object if the iteration failed. After either `done`
    /// response the iterator is finished and further calls return `{done: true}` again.
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
