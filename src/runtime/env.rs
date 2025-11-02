use std::collections::HashMap;
use std::rc::Rc;

use super::value::RuntimeValue;

#[derive(Clone, Default)]
pub struct Environment {
    bindings: Rc<HashMap<String, RuntimeValue>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: Rc::new(HashMap::new()),
        }
    }

    pub fn from_map(map: HashMap<String, RuntimeValue>) -> Self {
        Self {
            bindings: Rc::new(map),
        }
    }

    pub fn get(&self, name: &str) -> Option<RuntimeValue> {
        self.bindings.get(name).cloned()
    }

    pub fn extend(&self, name: impl Into<String>, value: RuntimeValue) -> Self {
        let mut map = (*self.bindings).clone();
        map.insert(name.into(), value);
        Self {
            bindings: Rc::new(map),
        }
    }

    pub fn extend_many<I, K>(&self, entries: I) -> Self
    where
        I: IntoIterator<Item = (K, RuntimeValue)>,
        K: Into<String>,
    {
        let mut map = (*self.bindings).clone();
        for (key, value) in entries {
            map.insert(key.into(), value);
        }
        Self {
            bindings: Rc::new(map),
        }
    }
}
