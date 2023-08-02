use crate::arena::Arena;
use crate::frontend::Compiler;
use crate::ir::{expr, Block, Expr, Immediate, Location, Op, Operand, Program, Width};

fn compile_nes_cheat(address: u16, key: u8, value: u8) -> Program<u16> {
    let mut exprs = Arena::new();
    let mut blocks = Arena::new();
    let assign = Block::new([
        Op::WriteImmediate {
            value: value.into(),
            dest: Location::Address(address),
        },
        Op::Return,
    ]);

    let assign = blocks.append(assign);

    let cmp = expr! {exprs,
      (imm key) == (load w Location::Address(address))
    };

    let expression = exprs.append(cmp);

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

#[cfg(test)]
mod test {
    use crate::frontend::game_genie_nes::GameGenieNes;
    use crate::frontend::Compiler;

    #[test]
    pub fn parse() {
        let program = GameGenieNes::compile(String::from(""));

        println!("{}", program);
    }
}
