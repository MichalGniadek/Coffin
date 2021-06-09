use std::ops::{Index, IndexMut};
use logos::Span;
use crate::ast::Id;

#[derive(Debug, Clone)]
pub struct SpansTable(Vec<Span>);

impl SpansTable {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, value: Span) -> Id {
        self.0.push(value);
        Id(self.0.len() as usize - 1)
    }
}

impl Index<Id> for SpansTable {
    type Output = Span;

    fn index(&self, index: Id) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl IndexMut<Id> for SpansTable {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}