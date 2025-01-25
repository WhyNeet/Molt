use std::{cell::RefCell, rc::Rc};

use lir::{block::BasicBlock, statement::Statement};

#[derive(Debug, Clone, Default)]
pub struct FunctionBuilder {
    blocks: Rc<RefCell<Vec<BasicBlock>>>,
    current: RefCell<u64>,
}

impl FunctionBuilder {
    pub fn new() -> Self {
        Self::default()
    }
}

impl FunctionBuilder {
    pub fn append_block(&self) -> u64 {
        let mut blocks = self.blocks.borrow_mut();

        let block_id = blocks.len() as u64;
        let block = BasicBlock::default();
        blocks.push(block);
        *self.current.borrow_mut() = block_id;

        block_id
    }

    pub fn position_at_end(&self, id: u64) {
        if id >= self.blocks.borrow().len() as u64 {
            panic!("no block with id: `{id}`")
        }

        *self.current.borrow_mut() = id;
    }

    pub fn append(&self, stmts: &mut Vec<Rc<Statement>>) {
        self.blocks
            .borrow()
            .get(*self.current.borrow() as usize)
            .unwrap()
            .append(stmts);
    }

    pub fn push(&self, stmt: Rc<Statement>) {
        self.blocks
            .borrow()
            .get(*self.current.borrow() as usize)
            .unwrap()
            .push(stmt);
    }

    pub fn into_blocks(&self) -> Vec<BasicBlock> {
        self.blocks.take()
    }
}
