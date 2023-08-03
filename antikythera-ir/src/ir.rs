use crate::arena::{Arena, Handle};
use std::fmt::{Debug, Display, Formatter, Write};

pub trait AddressSize: Debug + bincode::Encode + bincode::Decode + 'static {}

impl AddressSize for u8 {}
impl AddressSize for u16 {}
impl AddressSize for u32 {}
impl AddressSize for u64 {}

/// An untyped immediate value.
#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Immediate {
    Imm8(u8),
    Imm16(u16),
    Imm32(u32),
    Imm64(u64),
}

impl Display for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("im ")?;
        match self {
            Immediate::Imm8(v) => f.write_fmt(format_args!("8 0x{v:x}")),
            Immediate::Imm16(v) => f.write_fmt(format_args!("16 0x{v:x}")),
            Immediate::Imm32(v) => f.write_fmt(format_args!("32 0x{v:x}")),
            Immediate::Imm64(v) => f.write_fmt(format_args!("64 0x{v:x}")),
        }
    }
}

impl From<u8> for Immediate {
    fn from(value: u8) -> Self {
        Immediate::Imm8(value)
    }
}

impl From<u16> for Immediate {
    fn from(value: u16) -> Self {
        Immediate::Imm16(value)
    }
}

impl From<u32> for Immediate {
    fn from(value: u32) -> Self {
        Immediate::Imm32(value)
    }
}

impl From<u64> for Immediate {
    fn from(value: u64) -> Self {
        Immediate::Imm64(value)
    }
}

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Width {
    /// The full width of the program address size.
    /// For address size 8, this is equivalent to byte width.
    Word,
    /// Half the width of the program address size.
    /// For address size 16 and below, this is equivalent to byte width.
    Half,
    /// 1 fourth the width of the program address size.
    /// For address size 32 and below, this is equivalent to byte width
    Quarter,
    /// An absolute length of 1 byte (8 bits) wide.
    Byte,
}

/// A location in working memory.
///
/// If a register is provided, the location is loaded from
/// the given register interpreted as an unsigned integer
/// of address-sized width.
#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Location<A: AddressSize> {
    /// Load the address from the given register.
    Register(Register),
    /// Use the statically known address.
    Address(A),
    /// Use the location defined as the given register + offset
    Offset(Register, A),
}

impl<A: AddressSize> Display for Location<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Register(r) => f.write_fmt(format_args!("%{}", r.0)),
            Location::Address(a) => f.write_fmt(format_args!("@{a:x?}")),
            Location::Offset(r, a) => f.write_fmt(format_args!("@{a:x?}[%{0}]", r.0)),
        }
    }
}

/// An expression operand
#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Operand<A: AddressSize> {
    /// Load the value at the location with the given width.
    Load(Location<A>, Width),
    /// Use the raw value of the register.
    Register(Register),
    /// Use the immediate value.
    Immediate(Immediate),
}

impl Display for Width {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let w = match self {
            Width::Byte => "b",
            Width::Quarter => "q",
            Width::Half => "h",
            Width::Word => "w",
        };
        f.write_str(w)
    }
}

impl<A: AddressSize> Display for Operand<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Load(l, w) => f.write_fmt(format_args!("ld {w} {l}")),
            Operand::Register(r) => f.write_fmt(format_args!("rx {r}")),
            Operand::Immediate(i) => f.write_fmt(format_args!("{i}")),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode, Clone, Copy)]
pub struct Register(pub u8);
impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("%{}", self.0))
    }
}
#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Overflow {
    Wrapping,
    Saturating,
}

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum MathOp {
    Add,
    Sub,
    Mul,
    Div,
    Shl,
    Shr,
}

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum NumType {
    Integer,
    Floating,
}

/// Represents an operation on the antikythera virtual machine.
///
/// - A cheat with address size `A` has access to 256 persistent
///   registers (`u8::MAX`) of width `A`
#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Op<A: AddressSize> {
    /// Do nothing.
    NoOperation,

    // Register operations
    /// Load the given immediate value into the given register.
    LoadImmediate { value: A, register: Register },
    /// Load lhs and rhs, do the arithmetic operation according to the overflow mode,
    /// and write the output to the provided output register.
    ///
    /// If loads are less than register length, they will be zero-padded and the result will be
    /// on the zero-padded register length integer.
    Math {
        lhs: Operand<A>,
        rhs: Operand<A>,
        output: Register,
        op: MathOp,
        overflow: Overflow,
        load_type: NumType,
        store_type: NumType,
    },
    /// Load the value at the guest address into the register.
    LoadMemory {
        source: Location<A>,
        register: Register,
        width: Width,
    },
    /// Write the value stored in the register into the location `dest`
    Store {
        dest: Location<A>,
        register: Register,
        width: Width,
    },

    // Memory operations
    /// Write an immediate value to the destination.
    ///
    /// Cheat operations involving 8, 16, 32, and 64 bit immediate writes
    /// desugar to this operation.
    WriteImmediate { value: Immediate, dest: Location<A> },
    /// Write the provided vector of bytes into the destination.
    WriteBuffer { value: Vec<u8>, dest: Location<A> },

    /// Copy `length` bytes from the guest location at `source` to the guest location at `dest`.
    ///
    /// Cheat operations involving indirect writes desugar to this operation.
    MemoryCopy {
        source: Location<A>,
        dest: Location<A>,
        length: u64,
    },
    /// Beginning at `dest`, fill the memory with the provided immediate value `length` times
    MemoryFill {
        value: Immediate,
        dest: Location<A>,
        length: u64,
    },

    /// Branch to the block if the condition evaluates to true.
    /// If there is no condition, then the branch is unconditional, and always takes the accept branch.
    Branch {
        cond: Option<Handle<Expr<A>>>,
        target: Handle<Block<A>>,
    },

    /// Immediately stop execution of the program.
    Return,

    /// Immediately stop execution of the program and notify the caller.
    /// This is generally used for operations not supported by the antikythera virtual machine.
    ///
    /// An optional argument is provided to indicate the invalid guest opcode compiled.
    Exception(Option<A>),
}

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub struct Block<A: AddressSize>(Vec<Op<A>>);
impl<A: AddressSize> Block<A> {
    pub fn new(instrs: impl IntoIterator<Item = Op<A>>) -> Self {
        Block(instrs.into_iter().collect())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<A: AddressSize> Default for Block<A> {
    fn default() -> Self {
        Self::new([])
    }
}

impl Display for NumType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NumType::Integer => f.write_str("i"),
            NumType::Floating => f.write_str("f"),
        }
    }
}

impl Display for MathOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            MathOp::Add => "add",
            MathOp::Sub => "sub",
            MathOp::Mul => "mul",
            MathOp::Div => "div",
            MathOp::Shl => "shl",
            MathOp::Shr => "shr",
        };

        f.write_str(op)
    }
}

impl<A: AddressSize> Display for Op<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::NoOperation => f.write_str("noop"),
            Op::LoadImmediate { value, register } => {
                f.write_fmt(format_args!("{register} = im 0x{value:x?}"))
            }
            Op::Math {
                rhs,
                output,
                op,
                load_type,
                overflow,
                lhs,
                store_type,
            } => {
                let overflow = match overflow {
                    Overflow::Wrapping => "",
                    Overflow::Saturating => " sat",
                };

                f.write_fmt(format_args!(
                    "{output}{store_type} = {op}{load_type} {lhs} {rhs}{overflow}",
                ))
            }
            Op::LoadMemory {
                source,
                register,
                width,
            } => f.write_fmt(format_args!("{register} = l {width} @{source}")),
            Op::Store {
                dest,
                register,
                width,
            } => f.write_fmt(format_args!("{dest} = reg {width} {register}")),
            Op::WriteImmediate { value, dest } => f.write_fmt(format_args!("{dest} = {value}")),
            Op::WriteBuffer { value, dest } => {
                f.write_fmt(format_args!("memcpy {dest} im {value:?}"))
            }
            Op::MemoryCopy {
                source,
                dest,
                length,
            } => f.write_fmt(format_args!("memcpy {dest} {source} {length}")),
            Op::MemoryFill {
                value,
                dest,
                length,
            } => f.write_fmt(format_args!("memfill {dest} {value} {length}")),
            Op::Branch { cond, target } => {
                if let Some(cond) = cond {
                    f.write_fmt(format_args!("br {target:?} {cond:?}"))
                } else {
                    f.write_fmt(format_args!("br {target:?}"))
                }
            }
            Op::Return => f.write_str("ret"),
            Op::Exception(e) => {
                if let Some(e) = e {
                    f.write_fmt(format_args!("ex {e:?}"))
                } else {
                    f.write_str("ex")
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Program<A: AddressSize> {
    pub entry: Handle<Block<A>>,
    pub blocks: Arena<Block<A>>,
    pub exprs: Arena<Expr<A>>,
}

pub struct ProgramBuilder<A: AddressSize> {
    pub blocks: Arena<Block<A>>,
    pub exprs: Arena<Expr<A>>,
}

impl<A: AddressSize> ProgramBuilder<A> {
    pub fn new() -> Self {
        let exprs = Arena::new();
        let mut blocks = Arena::new();
        Self { blocks, exprs }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Expr<A: AddressSize> {
    Equal(Handle<Expr<A>>, Handle<Expr<A>>),
    GreaterThan(Handle<Expr<A>>, Handle<Expr<A>>),
    GreaterThanEqual(Handle<Expr<A>>, Handle<Expr<A>>),
    LessThan(Handle<Expr<A>>, Handle<Expr<A>>),
    LessThanEqual(Handle<Expr<A>>, Handle<Expr<A>>),
    And(Handle<Expr<A>>, Handle<Expr<A>>),
    Or(Handle<Expr<A>>, Handle<Expr<A>>),
    Not(Handle<Expr<A>>),
    Literal(Operand<A>),
}

macro_rules! expr {
    ($a:ident, im $op:expr) => {
        expr!(im $op)
    };
    (im $op:expr) => {
        $crate::ir::Expr::Literal($crate::ir::Operand::Immediate($crate::ir::Immediate::from($op)))
    };
    ($a:ident, rx $op:expr) => {
        expr!(rx $op)
    };
    (rx $op:expr) => {
        $crate::ir::Expr::Literal($crate::ir::Operand::Register($op))
    };
    (ld w $reg:expr) => {
        $crate::ir::Expr::Literal($crate::ir::Operand::Load($reg, $crate::ir::Width::Word))
    };
    ($a:ident, ld w $reg:expr) => {
        expr!(ld w $reg)
    };
    (ld h $reg:expr) => {
        $crate::ir::Expr::Literal($crate::ir::Operand::Load($reg, $crate::ir::Width::Half))
    };
    ($a:ident, ld h $reg:expr) => {
        expr!(ld h $reg)
    };
    (ld q $reg:expr) => {
        $crate::ir::Expr::Literal($crate::ir::Operand::Load($reg, $crate::ir::Width::Quarter))
    };
    ($a:ident, ld q $reg:expr) => {
        expr!(ld q $reg)
    };
    (ld b $reg:expr) => {
        $crate::ir::Expr::Literal($crate::ir::Operand::Load($reg, $crate::ir::Width::Byte))
    };
    ($a:ident, ld b $reg:expr) => {
        expr!(ld b $reg)
    };
    ($a:ident, ($($lhs:tt)*) == ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            $crate::ir::Expr::Equal(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) != ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            let eq = $crate::ir::Expr::Equal(lhs, rhs);
            let eq = $a.append(eq);
            $crate::ir::Expr::Not(eq)
        }
    };
    ($a:ident, ($($lhs:tt)*) > ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            $crate::ir::Expr::GreaterThan(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) >= ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            $crate::ir::Expr::GreaterThanEqual(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) < ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            $crate::ir::Expr::LessThan(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) <= ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            $crate::ir::Expr::LessThanEqual(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) & ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            $crate::ir::Expr::And(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) | ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            $crate::ir::Expr::Or(lhs, rhs)
        }
    };
    ($a:ident, !$($lhs:tt)*) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            $crate::ir::Expr::Not(lhs)
        }
    };
}

fn debug_expr<A: AddressSize>(arena: &Arena<Expr<A>>, expr: &Expr<A>) -> String {
    match expr {
        &Expr::Equal(l, r) => format!(
            "({} == {})",
            debug_expr(arena, arena.try_get(l).unwrap()),
            debug_expr(arena, arena.try_get(r).unwrap())
        ),
        &Expr::GreaterThan(l, r) => format!(
            "({} > {})",
            debug_expr(arena, arena.try_get(l).unwrap()),
            debug_expr(arena, arena.try_get(r).unwrap())
        ),
        &Expr::GreaterThanEqual(l, r) => format!(
            "({} >= {})",
            debug_expr(arena, arena.try_get(l).unwrap()),
            debug_expr(arena, arena.try_get(r).unwrap())
        ),
        &Expr::LessThan(l, r) => format!(
            "({} < {})",
            debug_expr(arena, arena.try_get(l).unwrap()),
            debug_expr(arena, arena.try_get(r).unwrap())
        ),
        &Expr::LessThanEqual(l, r) => format!(
            "({} <= {})",
            debug_expr(arena, arena.try_get(l).unwrap()),
            debug_expr(arena, arena.try_get(r).unwrap())
        ),
        &Expr::And(l, r) => format!(
            "({} & {})",
            debug_expr(arena, arena.try_get(l).unwrap()),
            debug_expr(arena, arena.try_get(r).unwrap())
        ),
        &Expr::Or(l, r) => format!(
            "({} | {})",
            debug_expr(arena, arena.try_get(l).unwrap()),
            debug_expr(arena, arena.try_get(r).unwrap())
        ),
        &Expr::Not(l) => format!("!({})", debug_expr(arena, arena.try_get(l).unwrap())),
        Expr::Literal(l) => format!("({l})"),
    }
}

impl<A: AddressSize> Display for Arena<Expr<A>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut expr_debug = f.debug_map();
        for (handle, expr) in self.iter() {
            expr_debug.key(&handle).value(&debug_expr(&self, expr));
        }

        expr_debug.finish()
    }
}

fn print_code<A: AddressSize>(
    f: &mut Formatter<'_>,
    blocks: &Arena<Block<A>>,
    exprs: &Arena<Expr<A>>,
) -> std::fmt::Result {
    for (handle, block) in blocks.iter() {
        f.write_fmt(format_args!("{handle:?}:\n"))?;
        for op in block.0.iter() {
            if let Op::Branch {
                cond: Some(cond),
                target,
            } = op
            {
                let expr = exprs.try_get(*cond).unwrap();
                f.write_fmt(format_args!(
                    "\tbr {target:?} [{}]\n",
                    debug_expr(exprs, expr)
                ))?;
            } else {
                f.write_fmt(format_args!("\t{op}\n"))?;
            }
        }
    }

    Ok(())
}

impl<A: AddressSize> Display for Program<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("entry: {:?}\n", self.entry))?;
        print_code(f, &self.blocks, &self.exprs)
    }
}

#[cfg(test)]
mod test {
    use crate::ir::*;
    #[test]
    pub fn test() {
        let reg = Location::Register(Register(0));
        let mut arena: Arena<Expr<u32>> = Arena::new();

        let expr: Expr<u32> = expr!(arena,
            (ld w reg) != (im 1u8)
        );
        println!("{:?}", debug_expr(&arena, &expr))
    }
}

pub(crate) use expr;
