use crate::arena::{Arena, Handle};
use crate::ir::{
    expr, Block, Expr, Immediate, Location, MathOp, NumType, Op, Operand, Overflow, Program,
    ProgramBuilder, Register, Width,
};

pub fn emit_assign_word_0(offset_reg: Register, address: u32, value: u32) -> Op<u32> {
    let dest = Location::Offset(offset_reg, address);
    Op::WriteImmediate {
        value: Immediate::Imm32(value),
        dest,
    }
}

pub fn emit_assign_half_1(offset_reg: Register, address: u32, value: u16) -> Op<u32> {
    let dest = Location::Offset(offset_reg, address);
    Op::WriteImmediate {
        value: Immediate::Imm16(value),
        dest,
    }
}

pub fn emit_assign_byte_2(offset_reg: Register, address: u32, value: u8) -> Op<u32> {
    let dest = Location::Offset(offset_reg, address);
    Op::WriteImmediate {
        value: Immediate::Imm8(value),
        dest,
    }
}

pub fn emit_if_lt_3(
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
        (im cmp) > (ld w loc)
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target: target,
    }
}

pub fn emit_if_gt_4(
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
        (im cmp) < (ld w loc)
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

pub fn emit_if_eq_5(
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
        (im cmp) == (ld w loc)
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

pub fn emit_if_neq_6(
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
        (im cmp) != (ld w loc)
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

pub fn emit_if_lt_mask_7(
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
        (im cmp) > (!(im mask) & (ld h loc))
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

pub fn emit_if_gt_mask_8(
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
        (im cmp) < (!(im mask) & (ld h loc))
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

pub fn emit_if_eq_mask_9(
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
        (im cmp) == (!(im mask) & (ld h loc))
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target: target,
    }
}

pub fn emit_if_neq_mask_a(
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
        (im cmp) != (!(im mask) & (ld h loc))
    );

    let expr = exprs.append(expr);

    Op::Branch {
        cond: Some(expr),
        target,
    }
}

pub fn emit_assign_register_offset(
    offset_reg: Register,
    store_reg: Register,
    address: u32,
    width: Width,
) -> Op<u32> {
    let loc = Location::Offset(offset_reg, address);
    Op::LoadMemory {
        source: loc,
        register: store_reg,
        width,
    }
}

pub fn emit_assign_reg_imm_d3_d5(register: Register, immediate: u32) -> Op<u32> {
    Op::LoadImmediate {
        value: immediate,
        register,
    }
}

pub fn emit_add_reg_imm_d4_dc(register: Register, immediate: u32) -> Op<u32> {
    Op::Math {
        lhs: Operand::Register(register),
        rhs: Operand::Immediate(Immediate::Imm32(immediate)),
        output: register,
        op: MathOp::Add,
        overflow: Overflow::Wrapping,
        load_type: NumType::Integer,
        store_type: NumType::Integer,
    }
}

pub fn emit_assign_word_offset_incr_d6(
    datareg: Register,
    offset: Register,
    address: u32,
) -> [Op<u32>; 2] {
    let assign = Op::Store {
        dest: Location::Offset(offset, address),
        register: datareg,
        width: Width::Word,
    };

    let increment = Op::Math {
        lhs: Operand::Register(offset),
        rhs: Operand::Immediate(Immediate::Imm32(4)),
        output: offset,
        op: MathOp::Add,
        overflow: Overflow::Wrapping,
        load_type: NumType::Integer,
        store_type: NumType::Integer,
    };

    [assign, increment]
}

pub fn emit_assign_half_offset_incr_d7(
    datareg: Register,
    offset: Register,
    address: u32,
) -> [Op<u32>; 2] {
    let assign = Op::Store {
        dest: Location::Offset(offset, address),
        register: datareg,
        width: Width::Half,
    };

    let increment = Op::Math {
        lhs: Operand::Register(offset),
        rhs: Operand::Immediate(Immediate::Imm32(2)),
        output: offset,
        op: MathOp::Add,
        overflow: Overflow::Wrapping,
        load_type: NumType::Integer,
        store_type: NumType::Integer,
    };

    [assign, increment]
}

pub fn emit_assign_byte_offset_incr_d8(
    datareg: Register,
    offset: Register,
    address: u32,
) -> [Op<u32>; 2] {
    let assign = Op::Store {
        dest: Location::Offset(offset, address),
        register: datareg,
        width: Width::Byte,
    };

    let increment = Op::Math {
        lhs: Operand::Register(offset),
        rhs: Operand::Immediate(Immediate::Imm32(1)),
        output: offset,
        op: MathOp::Add,
        overflow: Overflow::Wrapping,
        load_type: NumType::Integer,
        store_type: NumType::Integer,
    };

    [assign, increment]
}

pub fn emit_load_memory_d9_da_db(
    datareg: Register,
    offset_reg: Register,
    address: u32,
    width: Width,
) -> Op<u32> {
    Op::LoadMemory {
        source: Location::Offset(offset_reg, address),
        register: datareg,
        width,
    }
}

pub fn emit_copy_e(offset_reg: Register, offset: u32, bytes: Vec<u8>) -> Op<u32> {
    Op::WriteBuffer {
        value: bytes,
        dest: Location::Offset(offset_reg, offset),
    }
}

pub fn emit_copy_f(offset_reg: Register, offset: u32, length: u32) -> Op<u32> {
    Op::MemoryCopy {
        source: Location::Register(offset_reg),
        dest: Location::Address(offset),
        length: length as u64,
    }
}

/// c4 code not supported.
pub fn emit_c4() -> Op<u32> {
    Op::Exception(Some(0xC4000000))
}
