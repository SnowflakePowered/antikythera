use std::num::ParseIntError;
use nom::{IResult, Parser};
use nom::bytes::complete::{tag, take_while_m_n};
use nom::combinator::map_res;
use num_traits::{Num, Unsigned};
use crate::frontend::Compiler;


fn from_hex<N: Num<FromStrRadixErr = ParseIntError>  + Unsigned>(input: &str) -> Result<N, ParseIntError> {
    N::from_str_radix(input, 16)
}

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn take_imm<N: Num<FromStrRadixErr = ParseIntError> + Unsigned>(length: usize) -> impl Fn(&str) -> IResult<&str, N> {
    move |input| {
        let (input, address) = map_res(
            take_while_m_n(length, length, is_hex_digit),
            from_hex
        )(input)?;

        Ok((input, address))
    }
}

/// Action Replay DS Code Types.
///
/// A stream of code-types can be lexed to create an AST.
#[derive(Clone)]
pub enum CodeType {
    Write32 {
        location: u32,
        value: u32
    },
    Write16 {
        location: u32,
        value: u16,
    },
    Write8 {
        location: u32,
        value: u8,
    },
    LessThan32 {
        location: u32,
        cmp: u32
    },
    GreaterThan32 {
        location: u32,
        cmp: u32
    },
    Equal32 {
        location: u32,
        cmp: u32
    },
    NotEqual32 {
        location: u32,
        cmp: u32
    },
    LessThan16 {
        location: u32,
        mask: u16,
        cmp: u16
    },
    GreaterThan16 {
        location: u32,
        mask: u16,
        cmp: u16
    },
    Equal16 {
        location: u32,
        mask: u16,
        cmp: u16
    },
    NotEqual16 {
        location: u32,
        mask: u16,
        cmp: u16
    },
    LoadOffset {
        location: u32,
    },
    Repeat {
        limit: u32
    },
    EndIf,
    EndRepeat,
    EndCode,
    LoadOffsetImmediate {
        immediate: u32,
    },
    IncrementData {
        operand: u32
    },
    LoadDataImmediate {
        immediate: u32
    },
    StoreIncr32 {
        address: u32
    },
    StoreIncr16 {
        address: u32
    },
    StoreIncr8 {
        address: u32
    },
    LoadData32 {
        address: u32
    },
    LoadData16 {
        address: u32
    },
    LoadData8 {
        address: u32
    },
    MemWrite {
        address: u32,
        buffer: Vec<u8>
    },
    MemCopy {
        address: u32,
        count: u32,
    }
}

macro_rules! parse_assign_code {
    ($prefix:literal, $fn_name:ident, $code:ident::$var:ident) => {
        fn $fn_name(input: &str) -> IResult<&str, CodeType> {
            let (input, _) = tag($prefix)(input)?;
            let (input, address) = take_imm(7)(input)?;
            let (input, _) = tag(" ")(input)?;
            let (input, value) = take_imm(8)(input)?;
            Ok((input, $code::$var {
                location: address,
                value,
            }))
        }
    }
}

parse_assign_code!("0", parse_code_0, CodeType::Write32);
parse_assign_code!("1", parse_code_1, CodeType::Write16);
parse_assign_code!("2", parse_code_2, CodeType::Write8);


#[cfg(test)]
mod test {

}
