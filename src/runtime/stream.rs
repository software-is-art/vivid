use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use super::value::ScalarValue;

pub type Tick = usize;

#[derive(Clone)]
pub struct Stream {
    inner: Rc<StreamInner>,
}

struct StreamInner {
    thunk: Rc<dyn Fn(Tick) -> ScalarValue>,
    cache: RefCell<HashMap<Tick, ScalarValue>>,
}

impl Stream {
    pub fn new<F>(thunk: F) -> Self
    where
        F: Fn(Tick) -> ScalarValue + 'static,
    {
        Self {
            inner: Rc::new(StreamInner {
                thunk: Rc::new(thunk),
                cache: RefCell::new(HashMap::new()),
            }),
        }
    }

    pub fn value_at(&self, tick: Tick) -> ScalarValue {
        if let Some(value) = self.inner.cache.borrow().get(&tick) {
            return value.clone();
        }
        let value = (self.inner.thunk)(tick);
        self.inner.cache.borrow_mut().insert(tick, value.clone());
        value
    }

    pub fn constant(value: ScalarValue) -> Self {
        Self::new(move |_| value.clone())
    }

    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(ScalarValue) -> ScalarValue + 'static,
    {
        let stream = self.clone();
        Self::new(move |tick| {
            let value = stream.value_at(tick);
            f(value)
        })
    }

    pub fn zip<F>(&self, other: Stream, f: F) -> Self
    where
        F: Fn(ScalarValue, ScalarValue) -> ScalarValue + 'static,
    {
        let left = self.clone();
        Self::new(move |tick| {
            let a = left.value_at(tick);
            let b = other.value_at(tick);
            f(a, b)
        })
    }
}

impl fmt::Debug for Stream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Stream")
    }
}
