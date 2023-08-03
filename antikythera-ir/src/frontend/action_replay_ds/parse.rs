use crate::frontend::Compiler;
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take, take_while_m_n};
use nom::character::complete::newline;
use nom::combinator::{eof, map_res};
use nom::multi::separated_list1;
use nom::{IResult, Parser};
use num_traits::{Num, Unsigned};
use std::num::ParseIntError;

fn from_hex<N: Num<FromStrRadixErr = ParseIntError> + Unsigned>(
    input: &str,
) -> Result<N, ParseIntError> {
    N::from_str_radix(input, 16)
}

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn take_imm<N: Num<FromStrRadixErr = ParseIntError> + Unsigned>(
    length: usize,
) -> impl Fn(&str) -> IResult<&str, N> {
    move |input| {
        let (input, address) =
            map_res(take_while_m_n(length, length, is_hex_digit), from_hex)(input)?;

        Ok((input, address))
    }
}

/// Action Replay DS Code Types.
///
/// A stream of code-types can be lexed to create an AST.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CodeType {
    /// 0XXXXXXX YYYYYYYY
    Write32 {
        location: u32,
        value: u32,
    },
    /// 1XXXXXXX 0000YYYY
    Write16 {
        location: u32,
        value: u16,
    },
    /// 2XXXXXXX 000000YY
    Write8 {
        location: u32,
        value: u8,
    },
    /// 3XXXXXXX YYYYYYYY
    /// If location < cmp.
    LessThan32 {
        location: u32,
        cmp: u32,
    },
    /// 4XXXXXXX YYYYYYYY
    /// If location > cmp
    GreaterThan32 {
        location: u32,
        cmp: u32,
    },
    /// 5XXXXXXX YYYYYYYY
    Equal32 {
        location: u32,
        cmp: u32,
    },
    /// 6XXXXXXX YYYYYYYY
    NotEqual32 {
        location: u32,
        cmp: u32,
    },
    /// 7XXXXXXX ZZZZYYYY
    LessThan16 {
        location: u32,
        mask: u16,
        cmp: u16,
    },
    /// 8XXXXXXX ZZZZYYYY
    GreaterThan16 {
        location: u32,
        mask: u16,
        cmp: u16,
    },
    /// 9XXXXXXX ZZZZYYYY
    Equal16 {
        location: u32,
        mask: u16,
        cmp: u16,
    },
    NotEqual16 {
        location: u32,
        mask: u16,
        cmp: u16,
    },
    LoadOffset {
        location: u32,
    },
    Repeat {
        limit: u32,
    },
    EndIf,
    EndRepeat,
    /// D2XXXXXX ZZZZYYYY.
    ///
    EndCode,
    LoadOffsetImmediate {
        immediate: u32,
    },
    IncrementData {
        operand: u32,
    },
    LoadDataImmediate {
        immediate: u32,
    },
    StoreIncr32 {
        address: u32,
    },
    StoreIncr16 {
        address: u32,
    },
    StoreIncr8 {
        address: u32,
    },
    LoadData32 {
        address: u32,
    },
    LoadData16 {
        address: u32,
    },
    LoadData8 {
        address: u32,
    },
    IncrementOffset {
        operand: u32,
    },
    MemWrite {
        address: u32,
        buffer: Vec<u8>,
    },
    MemCopy {
        address: u32,
        count: u32,
    },
}

/// Create a parser for an assign code
macro_rules! parse_assign_code {
    ($prefix:literal, $width:literal, $fn_name:ident, $code:ident::$var:ident) => {
        fn $fn_name(input: &str) -> IResult<&str, CodeType> {
            let (input, _) = tag_no_case($prefix)(input)?;
            let (input, location) = take_imm(8 - $prefix.len())(input)?;
            let (input, _) = tag(" ")(input)?;

            let (input, _) = take(8 - $width)(input)?;
            let (input, value) = take_imm($width)(input)?;
            Ok((input, $code::$var { location, value }))
        }
    };
}

/// Create parser for a 32-bit conditional code
macro_rules! parse_cond32_code {
    ($prefix:literal, $fn_name:ident, $code:ident::$var:ident) => {
        fn $fn_name(input: &str) -> IResult<&str, CodeType> {
            let (input, _) = tag_no_case($prefix)(input)?;
            let (input, location) = take_imm(8 - $prefix.len())(input)?;
            let (input, _) = tag(" ")(input)?;
            let (input, cmp) = take_imm(8)(input)?;
            Ok((input, $code::$var { location, cmp }))
        }
    };
}

/// Create parser for conditional code with 16 bit mask.
macro_rules! parse_cond16_code {
    ($prefix:literal, $fn_name:ident, $code:ident::$var:ident) => {
        fn $fn_name(input: &str) -> IResult<&str, CodeType> {
            let (input, _) = tag_no_case($prefix)(input)?;
            let (input, location) = take_imm(8 - $prefix.len())(input)?;
            let (input, _) = tag(" ")(input)?;
            let (input, mask) = take_imm(4)(input)?;
            let (input, cmp) = take_imm(4)(input)?;

            Ok((
                input,
                $code::$var {
                    location,
                    mask,
                    cmp,
                },
            ))
        }
    };
}

/// Creates a parser that matches PXXXXXXX ????????
macro_rules! parse_hi_code {
    ($prefix:literal, $fn_name:ident, $code:ident::$var:ident { $value_name:ident }) => {
        fn $fn_name(input: &str) -> IResult<&str, CodeType> {
            let (input, _) = tag_no_case($prefix)(input)?;
            let (input, $value_name) = take_imm(8usize - $prefix.len())(input)?;
            let (input, _) = tag(" ")(input)?;
            let (input, _) = take(8usize)(input)?;

            Ok((input, $code::$var { $value_name }))
        }
    };
}

/// Creates a parser that matches P??????? XXXXXXXX
macro_rules! parse_lo_code {
    ($prefix:literal, $fn_name:ident, $code:ident::$var:ident { $value_name:ident }) => {
        fn $fn_name(input: &str) -> IResult<&str, CodeType> {
            let (input, _) = tag_no_case($prefix)(input)?;
            let (input, _) = take(8usize - $prefix.len())(input)?;
            let (input, _) = tag(" ")(input)?;
            let (input, $value_name) = take_imm(8)(input)?;

            Ok((input, $code::$var { $value_name }))
        }
    };
}

macro_rules! parse_terminator_code {
    ($prefix:literal, $fn_name:ident, $code:ident::$var:ident) => {
        fn $fn_name(input: &str) -> IResult<&str, CodeType> {
            let (input, _) = tag_no_case($prefix)(input)?;
            let (input, _) = take(8 - $prefix.len())(input)?;
            let (input, _) = tag(" ")(input)?;
            let (input, _) = take(8usize)(input)?;

            Ok((input, $code::$var))
        }
    };
}

parse_assign_code!("0", 8usize, parse_code_0, CodeType::Write32);
parse_assign_code!("1", 4usize, parse_code_1, CodeType::Write16);
parse_assign_code!("2", 2usize, parse_code_2, CodeType::Write8);
parse_cond32_code!("3", parse_code_3, CodeType::LessThan32);
parse_cond32_code!("4", parse_code_4, CodeType::GreaterThan32);
parse_cond32_code!("5", parse_code_5, CodeType::Equal32);
parse_cond32_code!("6", parse_code_6, CodeType::NotEqual32);
parse_cond16_code!("7", parse_code_7, CodeType::LessThan16);
parse_cond16_code!("8", parse_code_8, CodeType::GreaterThan16);
parse_cond16_code!("9", parse_code_9, CodeType::Equal16);
parse_cond16_code!("a", parse_code_a, CodeType::NotEqual16);
parse_hi_code!("b", parse_code_b, CodeType::LoadOffset { location });
parse_lo_code!("c", parse_code_c, CodeType::Repeat { limit });
parse_terminator_code!("d0", parse_code_d0, CodeType::EndIf);
parse_terminator_code!("d1", parse_code_d1, CodeType::EndRepeat);
parse_terminator_code!("d2", parse_code_d2, CodeType::EndCode);
parse_lo_code!(
    "d3",
    parse_code_d3,
    CodeType::LoadOffsetImmediate { immediate }
);
parse_lo_code!("d4", parse_code_d4, CodeType::IncrementData { operand });
parse_lo_code!(
    "d5",
    parse_code_d5,
    CodeType::LoadDataImmediate { immediate }
);
parse_lo_code!("d6", parse_code_d6, CodeType::StoreIncr32 { address });
parse_lo_code!("d7", parse_code_d7, CodeType::StoreIncr16 { address });
parse_lo_code!("d8", parse_code_d8, CodeType::StoreIncr8 { address });
parse_lo_code!("d9", parse_code_d9, CodeType::LoadData32 { address });
parse_lo_code!("da", parse_code_da, CodeType::LoadData16 { address });
parse_lo_code!("db", parse_code_db, CodeType::LoadData8 { address });
parse_lo_code!("dc", parse_code_dc, CodeType::IncrementOffset { operand });

fn parse_code_f(input: &str) -> IResult<&str, CodeType> {
    let (input, _) = tag_no_case("f")(input)?;
    let (input, address) = take_imm(7)(input)?;
    let (input, _) = tag(" ")(input)?;
    let (input, count) = take_imm(8)(input)?;
    Ok((input, CodeType::MemCopy { address, count }))
}

fn parse_code_e(input: &str) -> IResult<&str, CodeType> {
    fn parse_raw_memory_line(input: &str) -> IResult<&str, [u8; 8]> {
        // the bytes are little endian but the column is parsed from hi to low
        let (input, v3) = take_imm(2)(input)?;
        let (input, v2) = take_imm(2)(input)?;
        let (input, v1) = take_imm(2)(input)?;
        let (input, v0) = take_imm(2)(input)?;
        let (input, _) = tag(" ")(input)?;
        let (input, v7) = take_imm(2)(input)?;
        let (input, v6) = take_imm(2)(input)?;
        let (input, v5) = take_imm(2)(input)?;
        let (input, v4) = take_imm(2)(input)?;
        Ok((input, [v0, v1, v2, v3, v4, v5, v6, v7]))
    }

    let (input, _) = tag_no_case("e")(input)?;
    let (input, address) = take_imm(7)(input)?;
    let (input, _) = tag(" ")(input)?;
    let (input, length) = take_imm::<usize>(8)(input)?;

    let code_line_length = (length + 7) / 8; /* 2 bytes per line */

    let mut buffer = Vec::with_capacity(length + 4);

    let mut input = input;
    for _ in 0..code_line_length {
        let (line_input, _) = newline(input)?;
        let (line_input, bytes) = parse_raw_memory_line(line_input)?;
        input = line_input;
        buffer.extend_from_slice(&bytes)
    }

    /// After parsing all lines, there should be a newline.
    buffer.truncate(length);
    Ok((input, CodeType::MemWrite { address, buffer }))
}

fn parse_code_types(input: &str) -> IResult<&str, CodeType> {
    alt((
        parse_code_0,
        parse_code_1,
        parse_code_2,
        parse_code_3,
        parse_code_4,
        parse_code_5,
        parse_code_6,
        parse_code_7,
        parse_code_8,
        parse_code_9,
        parse_code_a,
        parse_code_b,
        parse_code_c,
        alt((
            parse_code_d0,
            parse_code_d1,
            parse_code_d2,
            parse_code_d3,
            parse_code_d4,
            parse_code_d5,
            parse_code_d6,
            parse_code_d7,
            parse_code_d8,
            parse_code_d9,
            parse_code_da,
            parse_code_db,
            parse_code_dc,
        )),
        parse_code_e,
        parse_code_f,
    ))(input)
}

pub fn parse(input: &str) -> IResult<&str, Vec<CodeType>> {
    let (input, list) = separated_list1(newline, parse_code_types)(input)?;
    let (input, _) = eof(input)?;
    Ok((input, list))
}

#[cfg(test)]
mod test {
    use crate::frontend::action_replay_ds::parse::{parse, parse_code_0};

    const LIST: &str = r##"021e0460 ffffffff
121e0484 00?0ffff"##;

    const E_TYPE: &str = r##"E2001234 00000010
DEADBEEF DEADBABE
DEB0D1ED F00DFEED
021e0460 ffffffff"##;

    #[test]
    pub fn parse_list() {
        let (_, list) = parse(LIST).unwrap();
        println!("{:x?}", list);
    }
    #[test]
    pub fn parse_e() {
        let (_, list) = parse(E_TYPE).unwrap();
        println!("{:x?}", list);
    }

    #[test]
    pub fn parse_one() {
        let (_, code) = parse_code_0("021e0460 ffffffff").unwrap();
        println!("{:x?}", code)
    }
}
