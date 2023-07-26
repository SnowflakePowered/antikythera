use crate::arena::Arena;
use crate::frontend::Compiler;
use crate::ir::{Block, BoolExpr, Immediate, Location, Op, Operand, Program};

fn compile_nes_cheat(address: u16, key: u8, value: u8) -> Program<u16> {
    let mut exprs = Arena::new();
    let mut blocks = Arena::new();
    let assign = Block::new([
        Op::WriteImmediate {
            value: Immediate::One(value),
            dest: Location::Address(address),
        },
        Op::Return,
    ]);

    let assign = blocks.append(assign);

    let cmp_key = exprs.append(BoolExpr::Literal(Operand::Immediate(Immediate::One(key))));
    let cmp_address = exprs.append(BoolExpr::Literal(Operand::Load(Location::Address(address))));
    let expression = exprs.append(BoolExpr::Equal(cmp_key, cmp_address));

    let cmp = Block::new([
        Op::Branch {
            cond: Some(expression),
            target: assign,
        },
        Op::Return,
    ]);

    let cmp = blocks.append(cmp);

    Program {
        entry: cmp,
        blocks,
        exprs,
    }
}

pub struct GameGenieNes;
impl Compiler<u16> for GameGenieNes {
    fn compile(cheat: String) -> Program<u16> {
        // todo: parse te code
        compile_nes_cheat(0, 0, 0)
    }
}
