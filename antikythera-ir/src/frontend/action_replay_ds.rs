use crate::arena::{Arena, Handle};
use crate::ir::{
    Block, BoolExpr, Immediate, Location, Op, Operand, Program, ProgramBuilder, Register, Width,
};

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
    exprs: &mut Arena<BoolExpr<u32>>,
) -> Op<u32> {
    let loc = if address == 0 {
        Location::Register(offset_reg)
    } else {
        Location::Address(address)
    };

    let lhs = exprs.append(BoolExpr::Literal(Operand::Immediate(Immediate::Four(cmp))));

    let rhs = exprs.append(BoolExpr::Literal(Operand::Load(loc, Width::Word)));

    let expr = exprs.append(BoolExpr::GreaterThan(lhs, rhs));

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
    exprs: &mut Arena<BoolExpr<u32>>,
) -> Op<u32> {
    let loc = if address == 0 {
        Location::Register(offset_reg)
    } else {
        Location::Address(address)
    };

    let lhs = exprs.append(BoolExpr::Literal(Operand::Immediate(Immediate::Two(cmp))));

    let rhs_not = exprs.append(BoolExpr::Not(
        exprs.append(BoolExpr::Literal(Operand::Immediate(Immediate::Two(mask)))),
    ));

    let rhs_cmp = exprs.append(BoolExpr::Literal(Operand::Load(loc, Width::Half)));

    let rhs = exprs.append(BoolExpr::And(rhs_not, rhs_cmp));

    let expr = exprs.append(BoolExpr::GreaterThan(lhs, rhs));

    Op::Branch {
        cond: Some(expr),
        target,
    }
}
