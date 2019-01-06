use super::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Rc<Object>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: &Rc<RefCell<Environment>>) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(outer.clone()),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        let got = self.store.get(name).map(|o| o.clone());
        if got.is_some() {
            return got;
        }

        match &self.outer {
            Some(o) => o.borrow().get(name),
            _ => None,
        }
    }

    pub fn set(&mut self, name: &str, value: Rc<Object>) -> Rc<Object> {
        self.store.insert(name.into(), value);
        self.store[name].clone()
    }
}
