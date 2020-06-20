use std::collections::HashMap;
use std::borrow::BorrowMut;
use crate::ast::TypeDecl;

#[derive(Debug, PartialEq, Clone)]
pub struct Symbols {
    pub scopes: Vec<Vec<String>>,
    pub table: HashMap<String, Vec<TypeDecl>>,
}


impl Symbols {
    pub fn new() -> Self {
        let symbol = Symbols {
            scopes: vec![],
            table: HashMap::new()
        };
        //symbol.begin_scope();
        symbol
    }


    pub fn begin_scope(&mut self)  {
        self.scopes.push(vec![])
    }

    pub fn end_scope(&mut self) {
        for identifier in self.scopes.last().expect("empty scopes") {
            if let Some(vars) = self.table.get_mut(identifier).borrow_mut() {
                vars.pop();
            } else {
                panic!("Err~~ not found var {}", identifier)
            }
        }
    }

    pub fn enter(&mut self, id: String, data: TypeDecl) {
        let bindings = self.table.entry(id.clone()).or_insert(vec![]);
        bindings.push(data);
        let current_scope = self.scopes.last_mut().expect("Err~~ scopes is empty!");
        current_scope.push(id);
    }

    pub fn look(&self, id: String) -> Option<&TypeDecl> {
        if let Some(bindings) = self.table.get(&id) {
            return bindings.last().to_owned();
        }
        None
    }


}