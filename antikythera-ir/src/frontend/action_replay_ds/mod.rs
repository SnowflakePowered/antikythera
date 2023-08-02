use crate::frontend::Compiler;
use crate::ir::Program;

mod compile;
mod emit;
mod parse;

/// Compiler for Datel Action Replay for Nintendo DS.
///
/// ## Register assignments
/// - `offset`: 0
/// - `stored`/`data`: 1
/// - `loopcount`: 2
///
/// ## Unsupported codes
/// - `C4??????`: Unsupported NitroHax code.
pub struct ActionReplayDS;
impl Compiler<u32> for ActionReplayDS {
    fn compile(cheat: String) -> Program<u32> {
        todo!();
    }
}
