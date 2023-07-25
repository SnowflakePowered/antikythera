use crate::arena::{Arena, Handle};
use crate::ir::{AddressSize, Block, Expression, Op};

mod action_replay_ds;
mod game_genie_nes;

pub trait Compiler<A: AddressSize> {
    fn compile(cheat: String) -> Program<A>;
}

pub struct Program<A: AddressSize> {
    pub entry: Handle<Block<A>>,
    pub blocks: Arena<Block<A>>,
    pub exprs: Arena<Expression<A>>,
}
