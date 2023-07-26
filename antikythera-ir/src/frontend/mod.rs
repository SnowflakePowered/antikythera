use crate::ir::{AddressSize, Program};

mod action_replay_ds;
mod game_genie_nes;

pub trait Compiler<A: AddressSize> {
    fn compile(cheat: String) -> Program<A>;
}
