use crate::{InterpreterError, functions};
use crate::objects::CelType;
use cel_parser::Expression;
use std::collections::HashMap;

pub struct Context {
    pub variables: HashMap<String, CelType>,
    pub functions:
        HashMap<String, Box<dyn Fn(Option<&CelType>, &[Expression], &Context) -> Result<CelType, InterpreterError>>>,
}

impl Context {
    pub fn add_variable(&mut self, name: String, value: CelType) {
        self.variables.insert(name, value);
    }

    pub fn add_function<F: 'static>(&mut self, name: String, value: F)
    where
        F: Fn(Option<&CelType>, &[Expression], &Context) -> Result<CelType, InterpreterError>,
    {
        self.functions.insert(name, Box::new(value));
    }
}

impl Default for Context {
    fn default() -> Self {
        let mut ctx = Context {
            variables: Default::default(),
            functions: Default::default(),
        };

        ctx.add_function("size".into(), |target, expr, context| {
            functions::size(target, expr, context)
        });

        ctx
    }
}
