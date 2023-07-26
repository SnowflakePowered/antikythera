use crate::arena::{Arena, Handle};
use crate::ir::{Block, Expr, expr, Immediate, Location, Op, Operand, Program, ProgramBuilder, Register, Width};

fn emit_assign_word(offset_reg: Register, address: u32, value: u32) -> Op<u32> {
    let dest = Location::Offset(offset_reg, address);
    return Op::WriteImmediate {
        value: Immediate::Four(value),
        dest,
    };
}

fn emit_assign_half(offset_reg: Register, address: u32, value: u16) -> Op<u32> {
    let dest = Location::Offset(offset_reg, address);
    return Op::WriteImmediate {
        value: Immediate::Two(value),
        dest,
    };
}

fn emit_assign_byte(offset_reg: Register, address: u32, value: u8) -> Op<u32> {
    let dest = Location::Offset(offset_reg, address);
    return Op::WriteImmediate {
        value: Immediate::One(value),
        dest,
    };
}

fn emit_if_gt_3(
    offset_reg: Register,
    address: u32,
    cmp: u32,
    target: Handle<Block<u32>>,
    exprs: &mut Arena<Expr<u32>>,
) -> Op<u32> {
    let loc = if address == 0 {
        Location::Register(offset_reg)
    } else {
        Location::Address(address)
    };

    let expr = expr!(exprs,
        (imm cmp) > (load w loc)
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

fn emit_if_gt_mask_7(
    offset_reg: Register,
    address: u32,
    mask: u16,
    cmp: u16,
    target: Handle<Block<u32>>,
    exprs: &mut Arena<Expr<u32>>,
) -> Op<u32> {
    let loc = if address == 0 {
        Location::Register(offset_reg)
    } else {
        Location::Address(address)
    };

    let expr = expr!(exprs,
        (imm cmp) > (!(imm mask) & (load h loc))
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}
