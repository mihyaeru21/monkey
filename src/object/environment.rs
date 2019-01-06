use super::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Rc<Object>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        self.store.get(name).map(|o| o.clone())
    }

    pub fn set(&mut self, name: &str, value: Rc<Object>) -> Rc<Object> {
        self.store.insert(name.into(), value);
        self.store[name].clone()
    }
}
