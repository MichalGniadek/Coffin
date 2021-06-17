use crate::ast::Id;
use logos::Span;
use std::ops::{Index, IndexMut};

#[derive(Debug, Clone)]
pub struct SpanTable(Vec<Span>);

impl SpanTable {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, value: Span) -> Id {
        self.0.push(value);
        Id::new(self.0.len() as usize - 1)
    }

    pub fn max_id(&self) -> Id {
        Id::new(self.0.len())
    }
}

impl Index<Id> for SpanTable {
    type Output = Span;

    fn index(&self, index: Id) -> &Self::Output {
        &self.0[usize::from(index)]
    }
}

impl IndexMut<Id> for SpanTable {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self.0[usize::from(index)]
    }
}
