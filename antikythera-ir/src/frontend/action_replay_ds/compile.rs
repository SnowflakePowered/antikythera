use crate::arena::{Arena, Handle};
use crate::frontend::action_replay_ds::emit;
use crate::frontend::action_replay_ds::emit::{
    emit_add_reg_imm_d4_dc, emit_assign_byte_offset_incr_d8, emit_assign_half_offset_incr_d7,
    emit_assign_reg_imm_d3_d5, emit_assign_word_offset_incr_d6, emit_if_eq_5, emit_if_eq_mask_9,
    emit_if_gt_4, emit_if_gt_mask_8, emit_if_lt_3, emit_if_lt_mask_7, emit_if_neq_6,
    emit_if_neq_mask_a, emit_load_memory_d9_da_db,
};
use crate::frontend::action_replay_ds::parse::CodeType;
use crate::ir::{expr, Block, Expr, Op, Operand, Program, Register, Width};
use std::slice::Iter;

const fn offset_reg() -> Register {
    Register(0)
}

const fn data_reg() -> Register {
    Register(1)
}

const fn loopcnt_reg() -> Register {
    Register(2)
}

pub fn parse(instrs: Vec<CodeType>) -> Program<u32> {
    let mut blocks: Arena<Block<u32>> = Arena::new();
    let mut exprs: Arena<Expr<u32>> = Arena::new();

    let offset_reg = Register(0);
    let data_reg = Register(1);
    let loop_reg = Register(2);

    let mut instrs = instrs.as_slice();
    let entry = compile_next_block(
        &mut instrs.iter(),
        &mut blocks,
        &mut exprs,
        offset_reg,
        data_reg,
        loop_reg,
        None,
        true,
    );

    let entry = blocks.append(entry);
    Program {
        entry,
        blocks,
        exprs,
    }
}

/// Munch the number of ops until either balanced EndIf, or an EndCode.
fn munch_conditional_body<'a>(instrs: &[CodeType]) -> &[CodeType] {
    let mut counter = 1;
    let mut index = 0;
    for instr in instrs {
        match instr {
            &CodeType::EndIf => {
                counter -= 1;
                index += 1;
            }
            &CodeType::EndCode => {
                index += 1;
                break;
            }
            &CodeType::GreaterThan16 { .. }
            | &CodeType::GreaterThan32 { .. }
            | &CodeType::LessThan16 { .. }
            | &CodeType::LessThan32 { .. }
            | &CodeType::Equal32 { .. }
            | &CodeType::Equal16 { .. }
            | &CodeType::NotEqual32 { .. }
            | &CodeType::NotEqual16 { .. } => {
                counter += 1;
                index += 1;
            }
            _ => index += 1,
        }

        if counter == 0 {
            break;
        }
    }

    instrs.split_at(index).0
}

/// Repeat is lazy and matches up to the first EndRepeat, Repeat, or EndCode
fn munch_repeat_body<'a>(instrs: &[CodeType]) -> &[CodeType] {
    let mut index = 0;
    for instr in instrs {
        match instr {
            /// Make next block start with repeat
            &CodeType::Repeat { .. } => {
                break;
            }
            &CodeType::EndRepeat
            // I might want to make endcode start with next block too...
            | &CodeType::EndCode => {
                index += 1;
                break;
            }
            _ => index += 1,
        }
    }

    instrs.split_at(index).0
}

fn handle_conditional(
    instrs: &mut Iter<CodeType>,
    current_block: &mut Vec<Op<u32>>,
    offset_reg: Register,
    data_reg: Register,
    loopcnt_reg: Register,
    blocks: &mut Arena<Block<u32>>,
    exprs: &mut Arena<Expr<u32>>,
    terminal: Option<Handle<Block<u32>>>,
    flush: bool,
    emit: impl FnOnce(Handle<Block<u32>>, &mut Arena<Expr<u32>>) -> Op<u32>,
) {
    let map = instrs.as_slice();
    let body = munch_conditional_body(map);

    for _ in 0..body.len() {
        let _ = instrs.next();
    }
    eprintln!("body\n:{:#x?}\n===", body);

    let continuation = compile_next_block(
        instrs,
        blocks,
        exprs,
        offset_reg,
        data_reg,
        loopcnt_reg,
        terminal,
        flush,
    );

    let continuation = if continuation.len() != 0 {
        Some(blocks.append(continuation))
    } else {
        terminal
    };

    let body = compile_next_block(
        &mut body.iter(),
        blocks,
        exprs,
        offset_reg,
        data_reg,
        loopcnt_reg,
        continuation,
        flush,
    );

    let body = blocks.append(body);

    current_block.push(emit(body, exprs));
    if let Some(continuation) = continuation {
        current_block.push(Op::Branch {
            cond: None,
            target: continuation,
        });
    } else {
        current_block.push(Op::Return)
    }
}

pub fn compile_next_block<'a>(
    mut instrs: &mut Iter<CodeType>,
    blocks: &mut Arena<Block<u32>>,
    exprs: &mut Arena<Expr<u32>>,
    offset_reg: Register,
    data_reg: Register,
    loopcnt_reg: Register,
    terminal: Option<Handle<Block<u32>>>,
    flush: bool,
) -> Block<u32> {
    let mut current_block = Vec::new();
    while let Some(instr) = instrs.next() {
        match instr {
            &CodeType::Write32 { location, value } => {
                current_block.push(emit::emit_assign_word_0(offset_reg, location, value))
            }
            &CodeType::Write16 { location, value } => {
                current_block.push(emit::emit_assign_half_1(offset_reg, location, value))
            }
            &CodeType::Write8 { location, value } => {
                current_block.push(emit::emit_assign_byte_2(offset_reg, location, value))
            }
            &CodeType::LessThan32 { location, cmp } => {
                handle_conditional(
                    instrs,
                    &mut current_block,
                    offset_reg,
                    data_reg,
                    loopcnt_reg,
                    blocks,
                    exprs,
                    terminal,
                    flush,
                    |body, exprs| emit_if_lt_3(offset_reg, location, cmp, body, exprs),
                );
            }
            &CodeType::GreaterThan32 { location, cmp } => {
                handle_conditional(
                    instrs,
                    &mut current_block,
                    offset_reg,
                    data_reg,
                    loopcnt_reg,
                    blocks,
                    exprs,
                    terminal,
                    flush,
                    |body, exprs| emit_if_gt_4(offset_reg, location, cmp, body, exprs),
                );
            }
            &CodeType::Equal32 { location, cmp } => {
                handle_conditional(
                    instrs,
                    &mut current_block,
                    offset_reg,
                    data_reg,
                    loopcnt_reg,
                    blocks,
                    exprs,
                    terminal,
                    flush,
                    |body, exprs| emit_if_eq_5(offset_reg, location, cmp, body, exprs),
                );
            }

            &CodeType::NotEqual32 { location, cmp } => {
                handle_conditional(
                    instrs,
                    &mut current_block,
                    offset_reg,
                    data_reg,
                    loopcnt_reg,
                    blocks,
                    exprs,
                    terminal,
                    flush,
                    |body, exprs| emit_if_neq_6(offset_reg, location, cmp, body, exprs),
                );
            }
            &CodeType::LessThan16 {
                location,
                cmp,
                mask,
            } => {
                handle_conditional(
                    instrs,
                    &mut current_block,
                    offset_reg,
                    data_reg,
                    loopcnt_reg,
                    blocks,
                    exprs,
                    terminal,
                    flush,
                    |body, exprs| emit_if_lt_mask_7(offset_reg, location, mask, cmp, body, exprs),
                );
            }
            &CodeType::GreaterThan16 {
                location,
                cmp,
                mask,
            } => {
                handle_conditional(
                    instrs,
                    &mut current_block,
                    offset_reg,
                    data_reg,
                    loopcnt_reg,
                    blocks,
                    exprs,
                    terminal,
                    flush,
                    |body, exprs| emit_if_gt_mask_8(offset_reg, location, mask, cmp, body, exprs),
                );
            }
            &CodeType::Equal16 {
                location,
                cmp,
                mask,
            } => {
                handle_conditional(
                    instrs,
                    &mut current_block,
                    offset_reg,
                    data_reg,
                    loopcnt_reg,
                    blocks,
                    exprs,
                    terminal,
                    flush,
                    |body, exprs| emit_if_eq_mask_9(offset_reg, location, mask, cmp, body, exprs),
                );
            }
            &CodeType::NotEqual16 {
                location,
                cmp,
                mask,
            } => {
                handle_conditional(
                    instrs,
                    &mut current_block,
                    offset_reg,
                    data_reg,
                    loopcnt_reg,
                    blocks,
                    exprs,
                    terminal,
                    flush,
                    |body, exprs| emit_if_neq_mask_a(offset_reg, location, mask, cmp, body, exprs),
                );
            }
            &CodeType::LoadOffset { .. } => {}
            &CodeType::Repeat { limit } => {
                eprintln!("========");

                let map = instrs.as_slice();
                let body = munch_repeat_body(map);
                eprintln!("body\n:{:#x?}\n\n", body);

                for _ in 0..body.len() {
                    let _ = instrs.next();
                }

                eprintln!("rest\n:{:#x?}\n\n", instrs);

                let continuation = compile_next_block(
                    instrs,
                    blocks,
                    exprs,
                    offset_reg,
                    data_reg,
                    loopcnt_reg,
                    terminal,
                    flush,
                );

                let continuation = if continuation.len() != 0 {
                    Some(blocks.append(continuation))
                } else {
                    terminal
                };

                let loopcnt_check = expr!(exprs,
                    (rx loopcnt_reg) < (im limit)
                );

                let loopcnt_check = exprs.append(loopcnt_check);

                let conditional = blocks.append_recursive(|blocks, conditional| {
                    let flush = flush && body.ends_with(&[CodeType::EndCode]);
                    // loop body block always goes back to condition check
                    // Do not flush within the body of the block
                    let body = compile_next_block(
                        &mut body.iter(),
                        blocks,
                        exprs,
                        offset_reg,
                        data_reg,
                        loopcnt_reg,
                        Some(conditional),
                        false,
                    );

                    let body = blocks.append(body);
                    eprintln!("body: [{body:?}]");

                    let mut current_block = Vec::new();
                    current_block.push(
                        // if *%loopcnt < limit br body
                        Op::Branch {
                            cond: Some(loopcnt_check),
                            target: body,
                        },
                    );


                    // Flush before breaking if the loop body ends with a EndCode.
                    if flush {
                        current_block.push(Op::LoadImmediate {
                            register: offset_reg,
                            value: 0,
                        });
                        current_block.push(Op::LoadImmediate {
                            register: data_reg,
                            value: 0,
                        });
                    }

                    current_block.push(if let Some(continuation) = continuation {
                        Op::Branch {
                            cond: None,
                            target: continuation,
                        }
                    } else {
                        Op::Return
                    });

                    Block::new(current_block)
                });

                eprintln!("rest: [{continuation:?}]");

                // Reset loopcounter
                current_block.push(Op::LoadImmediate {
                    value: 0,
                    register: loopcnt_reg,
                });

                // Unconditionally branch to the conditional.
                // this ensures that it always cancels out previous for loops.
                current_block.push(Op::Branch {
                    cond: None,
                    target: conditional,
                });

                eprintln!("========");
            }
            &CodeType::EndIf => {
                current_block.push(terminal.map_or(Op::Return, |block| Op::Branch {
                    cond: None,
                    target: block,
                }))
            }
            &CodeType::EndRepeat => {
                // increment loop counter
                current_block.push(emit_add_reg_imm_d4_dc(loopcnt_reg, 1));
                current_block.push(terminal.map_or(Op::Return, |block| Op::Branch {
                    cond: None,
                    target: block,
                }))
            }
            &CodeType::EndCode => {
                current_block.push(emit_add_reg_imm_d4_dc(loopcnt_reg, 1));
                if flush {
                    current_block.push(Op::LoadImmediate {
                        register: offset_reg,
                        value: 0,
                    });
                    current_block.push(Op::LoadImmediate {
                        register: data_reg,
                        value: 0,
                    });
                }
                current_block.push(terminal.map_or(Op::Return, |block| Op::Branch {
                    cond: None,
                    target: block,
                }))
            }
            &CodeType::LoadOffsetImmediate { immediate } => {
                current_block.push(emit_assign_reg_imm_d3_d5(offset_reg, immediate));
            }
            &CodeType::IncrementData { operand } => {
                current_block.push(emit_add_reg_imm_d4_dc(data_reg, operand));
            }
            &CodeType::LoadDataImmediate { immediate } => {
                current_block.push(emit_assign_reg_imm_d3_d5(data_reg, immediate));
            }
            &CodeType::StoreIncr32 { address } => current_block.extend(
                emit_assign_word_offset_incr_d6(data_reg, offset_reg, address),
            ),
            &CodeType::StoreIncr16 { address } => current_block.extend(
                emit_assign_half_offset_incr_d7(data_reg, offset_reg, address),
            ),
            &CodeType::StoreIncr8 { address } => current_block.extend(
                emit_assign_byte_offset_incr_d8(data_reg, offset_reg, address),
            ),
            &CodeType::LoadData32 { address } => current_block.push(emit_load_memory_d9_da_db(
                data_reg,
                offset_reg,
                address,
                Width::Word,
            )),
            &CodeType::LoadData16 { address } => current_block.push(emit_load_memory_d9_da_db(
                data_reg,
                offset_reg,
                address,
                Width::Half,
            )),
            &CodeType::LoadData8 { address } => current_block.push(emit_load_memory_d9_da_db(
                data_reg,
                offset_reg,
                address,
                Width::Byte,
            )),
            &CodeType::MemWrite { .. } => {}
            &CodeType::MemCopy { .. } => {}
            &CodeType::IncrementOffset { operand } => {
                current_block.push(emit_add_reg_imm_d4_dc(data_reg, operand));
            }
        }
    }

    Block::new(current_block)
}

#[cfg(test)]
mod test {
    use crate::frontend::action_replay_ds::parse;

    const LIST: &str = r##"021e0460 ffffffff
121e0484 00?0ffff"##;

    const COND: &str = r##"94000130 FFFB0000
C0000000 00000017
12242B48 000084A5
D0000000 0000001A"##;

    const CONTINUATION_AFTER_D2: &str = r##"94000130 FCFF0300
5200764C 1E523308
1200764C 0000C303
D2000000 00000000
94000130 FCFF0100
5200764C 1E52C303
1200764C 00003308
D2000000 00000000"##;

    const CONTINUATION_AFTER_NESTED_IF: &str = r##"94000130 FCFF0300
5200764C 1E523308
D0000000 00000000
1200764C 0000C303
D0000000 00000000
94000130 FCFF0100
5200764C 1E52C303
1200764C 00003308
D2000000 00000000"##;

    const REPEATS_EASY: &str = r##"94000130 fffb0000
c0000000 0000003f
12234846 00000384
d1000000 00000000
dc000000 00000004
d2000000 00000000
c0000000 0000003f
dc000000 00000002
d2000000 00000000"##;

    const REPEATS: &str = r##"94000130 fffb0000
c0000000 0000003f
12234846 00000384
d1000000 00000000
dc000000 00000004
d2000000 00000000
94000130 fffb0000
d5000000 00000095
c0000000 0000003f
d7000000 02234844
d4000000 00000001
dc000000 00000002
d2000000 00000000"##;

    const REPEATS_EASY_IF_INTERLEAVED: &str = r##"c0000000 0000003f
94000130 fffb0000
12234846 00000384
d2000000 00000000
dc000000 00000004
c0000000 0000003f
dc000000 00000002
d2000000 00000000"##;

    #[test]
    pub fn parse() {
        let (_, list) = parse::parse(LIST).unwrap();
        let program = super::parse(list);
        println!("{:x?}", program);
    }

    #[test]
    pub fn parse_cond() {
        let (_, list) = parse::parse(COND).unwrap();
        let program = super::parse(list);
        println!("{:#x?}", program);
    }

    #[test]
    pub fn parse_d2() {
        let (_, list) = parse::parse(CONTINUATION_AFTER_D2).unwrap();
        println!("{:#x?}", list);

        let program = super::parse(list);

        println!("{}", program);
    }

    #[test]
    pub fn parse_nested() {
        let (_, list) = parse::parse(CONTINUATION_AFTER_NESTED_IF).unwrap();
        println!("{:#x?}", list);

        let program = super::parse(list);

        println!("{}", program);
    }

    #[test]
    pub fn parse_repeats() {
        let (_, list) = parse::parse(REPEATS_EASY).unwrap();
        println!("{:#x?}", list);

        let program = super::parse(list);
        //
        println!("{}", program);
    }

    #[test]
    pub fn parse_repeats_interleaved() {
        let (_, list) = parse::parse(REPEATS_EASY_IF_INTERLEAVED).unwrap();
        println!("{:#x?}", list);

        let program = super::parse(list);
        //
        println!("{}", program);
    }
}
