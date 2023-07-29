use crate::arena::{Arena, Handle};
use crate::ir::{Block, Expr, expr, Immediate, Location, MathOp, NumType, Op, Operand, Overflow, Program, ProgramBuilder, Register, Width};

fn emit_assign_word_0(offset_reg: Register, address: u32, value: u32) -> Op<u32> {
    let dest = Location::Offset(offset_reg, address);
    Op::WriteImmediate {
        value: Immediate::Four(value),
        dest,
    }
}

fn emit_assign_half_1(offset_reg: Register, address: u32, value: u16) -> Op<u32> {
    let dest = Location::Offset(offset_reg, address);
    Op::WriteImmediate {
        value: Immediate::Two(value),
        dest,
    }
}

fn emit_assign_byte_2(offset_reg: Register, address: u32, value: u8) -> Op<u32> {
    let dest = Location::Offset(offset_reg, address);
    Op::WriteImmediate {
        value: Immediate::One(value),
        dest,
    }
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

fn emit_if_lt_4(
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
        (imm cmp) < (load w loc)
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

fn emit_if_eq_5(
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
        (imm cmp) == (load w loc)
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

fn emit_if_neq_6(
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
        (imm cmp) != (load w loc)
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

fn emit_if_lt_mask_8(
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
        (imm cmp) < (!(imm mask) & (load h loc))
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

fn emit_if_eq_mask_9(
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
        (imm cmp) == (!(imm mask) & (load h loc))
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

fn emit_if_neq_mask_a(
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
        (imm cmp) != (!(imm mask) & (load h loc))
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

fn emit_assign_register_offset(
    offset_reg: Register,
    store_reg: Register,
    address: u32,
    width: Width
) -> Op<u32> {
    let loc = Location::Offset(offset_reg, address);
    Op::LoadMemory {
        source: loc,
        register: store_reg,
        width
    }
}

fn emit_assign_reg_imm_d3_d5(
    register: Register,
    immediate: u32
) -> Op<u32> {
    Op::LoadImmediate {
        value: immediate,
        register,
    }
}

fn emit_add_reg_imm_d4(
    register: Register,
    immediate: u32
) -> Op<u32> {
    Op::Math {
        lhs: Operand::Register(register),
        rhs: Operand::Immediate(Immediate::Four(immediate)),
        output: register,
        op: MathOp::Add,
        overflow: Overflow::Wrapping,
        load_type: NumType::Integer,
        store_type: NumType::Integer,
    }
}

fn emit_assign_word_offset_incr_d6(
    datareg: Register,
    offset: Register,
    address: u32
) -> [Op<u32>; 2] {
    let assign = Op::Store {
        dest: Location::Offset(offset, address),
        register: datareg,
        width: Width::Word,
    };

    let increment = Op::Math {
        lhs: Operand::Register(offset),
        rhs: Operand::Immediate(Immediate::Four(4)),
        output: offset,
        op: MathOp::Add,
        overflow: Overflow::Wrapping,
        load_type: NumType::Integer,
        store_type: NumType::Integer,
    };

    [assign, increment]
}

fn emit_assign_half_offset_incr_d7(
    datareg: Register,
    offset: Register,
    address: u32
) -> [Op<u32>; 2] {
    let assign = Op::Store {
        dest: Location::Offset(offset, address),
        register: datareg,
        width: Width::Half,
    };

    let increment = Op::Math {
        lhs: Operand::Register(offset),
        rhs: Operand::Immediate(Immediate::Four(2)),
        output: offset,
        op: MathOp::Add,
        overflow: Overflow::Wrapping,
        load_type: NumType::Integer,
        store_type: NumType::Integer,
    };

    [assign, increment]
}

fn emit_assign_byte_offset_incr_d7(
    datareg: Register,
    offset: Register,
    address: u32
) -> [Op<u32>; 2] {
    let assign = Op::Store {
        dest: Location::Offset(offset, address),
        register: datareg,
        width: Width::Byte,
    };

    let increment = Op::Math {
        lhs: Operand::Register(offset),
        rhs: Operand::Immediate(Immediate::Four(1)),
        output: offset,
        op: MathOp::Add,
        overflow: Overflow::Wrapping,
        load_type: NumType::Integer,
        store_type: NumType::Integer,
    };

    [assign, increment]
}

fn emit_copy_e(
    offset_reg: Register,
    offset: u32,
    bytes: Vec<u8>
) -> Op<u32> {
    Op::WriteBuffer {
        value: bytes,
        dest: Location::Offset(offset_reg, offset),
    }
}

fn emit_copy_f(
    offset_reg: Register,
    offset: u32,
    length: u32
) -> Op<u32> {
    Op::MemoryCopy {
        source: Location::Register(offset_reg),
        dest: Location::Address(offset),
        length: length as u64,
    }
}
/// c4 code not supported.
fn emit_c4() -> Op<u32> {
    Op::Exception(Some(0xC4000000))
}