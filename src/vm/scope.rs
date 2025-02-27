use crate::common::lexer::token::Token;

#[derive(Debug, Clone)]
pub struct Local {
    pub name: Token,
    pub depth: i32,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub locals: Vec<Local>,
    pub depth: i32,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            locals: vec![],
            depth: 0,
        }
    }

    pub fn begin_scope(&mut self) {
        self.depth += 1;
    }

    pub fn end_scope(&mut self) -> u32 {
        self.depth -= 1;

        // Remove variables that are out of the current scope
        let mut counter = 0;
        self.locals.retain(|local| {
            if local.depth > self.depth {
                counter += 1;
                false
            } else {
                true
            }
        });

        counter
    }

    pub fn add_local(&mut self, name: Token) {
        let local = Local {
            name,
            depth: -1, // Mark uninitialized
        };
        self.locals.push(local);
    }

    pub fn resolve_local(&self, name: &Token) -> Option<usize> {
        // Check name and if it's initialized
        self.locals
            .iter()
            .rev()
            .position(|local| local.name.lexeme == *name.lexeme && local.depth > -1)
            .map(|rev_index| self.locals.len() - 1 - rev_index)
    }
}
