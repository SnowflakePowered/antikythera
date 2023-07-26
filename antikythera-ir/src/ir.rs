use crate::arena::{Arena, Handle};

pub trait AddressSize: bincode::Encode + bincode::Decode + 'static {}

impl AddressSize for u8 {}
impl AddressSize for u16 {}
impl AddressSize for u32 {}
impl AddressSize for u64 {}

/// An untyped immediate value.
#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Immediate {
    One(u8),
    Two(u16),
    Four(u32),
    Eight(u64),
}

impl From<u8> for Immediate {
    fn from(value: u8) -> Self {
        Immediate::One(value)
    }
}

impl From<u16> for Immediate {
    fn from(value: u16) -> Self {
        Immediate::Two(value)
    }
}

impl From<u32> for Immediate {
    fn from(value: u32) -> Self {
        Immediate::Four(value)
    }
}


impl From<u64> for Immediate {
    fn from(value: u64) -> Self {
        Immediate::Eight(value)
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

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Location<A: AddressSize> {
    /// Load the location from the given register.
    Register(Register),
    /// Use the statically known address.
    Address(A),
    /// Use the location defined as the given register + offset
    Offset(Register, A),
}

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Operand<A: AddressSize> {
    Load(Location<A>, Width),
    Immediate(Immediate),
}

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode, Clone, Copy)]
pub struct Register(u8);

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Overflow {
    Saturating,
    Wrapping,
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
    LoadImmediate {
        value: A,
        register: Register,
    },
    /// Do math on the register
    Math {
        input: Operand<A>,
        output: Register,
        op: MathOp,
        overflow: Overflow,
        load_type: NumType,
        store_type: NumType,
    },
    /// Load the value at the guest address into the register.
    LoadMemory {
        source: A,
        register: Register,
    },
    /// Write the value stored in the register into the location `dest`
    Store {
        dest: Location<A>,
        register: Register,
    },

    // Memory operations
    /// Write an immediate value to the destination.
    ///
    /// Cheat operations involving 8, 16, 32, and 64 bit immediate writes
    /// desugar to this operation.
    WriteImmediate {
        value: Immediate,
        dest: Location<A>,
    },
    /// Write the provided vector of bytes into the destination.
    WriteBuffer {
        value: Vec<u8>,
        dest: Location<A>,
    },

    /// Copy `length` bytes from the guest location at `source` to the guest location at `dest`.
    ///
    /// Cheat operations involving indirect writes desugar to this operation.
    MemoryCopy {
        source: Location<A>,
        dest: Location<A>,
        length: u64,
    },
    /// Beginning at `dest`, fill the memory with the provided immediate value `length` times, skipping
    /// offset
    MemoryFill {
        value: Immediate,
        dest: Location<A>,
        length: u64,
        offset: A,
    },

    // Branch to the block if the expression evaluates to true.
    Branch {
        cond: Option<Handle<Expr<A>>>,
        target: Handle<Block<A>>,
    },

    Return,
}

#[derive(Debug, Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub struct Block<A: AddressSize>(Vec<Op<A>>);
impl<A: AddressSize> Block<A> {
    pub fn new(instrs: impl IntoIterator<Item = Op<A>>) -> Self {
        Block(instrs.into_iter().collect())
    }
}

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

// macro_rules! imm {
//     ($val:ident u8) => {
//         Immediate::One($val)
//     };
//     ($val:literal u8) => {
//         Immediate::One($val)
//     };
//     ($val:ident u16) => {
//         Immediate::Two($val)
//     };
//     ($val:literal u16) => {
//         Immediate::Two($val)
//     };
//     ($val:ident u32) => {
//         Immediate::Four($val)
//     };
//     ($val:literal u32) => {
//         Immediate::Four($val)
//     };
//     ($val:ident u64) => {
//         Immediate::Eight($val)
//     };
//     ($val:literal u64) => {
//         Immediate::Eight($val)
//     };
// }

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
    ($a:ident, imm $op:expr) => {
        expr!(imm $op)
    };
    (imm $op:expr) => {
        Expr::Literal(Operand::Immediate(Immediate::from($op)))
    };
    (load w $reg:expr) => {
        Expr::Literal(Operand::Load($reg, Width::Word))
    };
    ($a:ident, load w $reg:expr) => {
        expr!(load w $reg)
    };
    (load h $reg:expr) => {
        Expr::Literal(Operand::Load($reg, Width::Half))
    };
    ($a:ident, load h $reg:expr) => {
        expr!(load h $reg)
    };
    (load q $reg:expr) => {
        Expr::Literal(Operand::Load($reg, Width::Quarter))
    };
    ($a:ident, load q $reg:expr) => {
        expr!(load q $reg)
    };
    (load b $reg:expr) => {
        Expr::Literal(Operand::Load($reg, Width::Byte))
    };
    ($a:ident, load b $reg:expr) => {
        expr!(load b $reg)
    };
    ($a:ident, ($($lhs:tt)*) == ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            Expr::Equal(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) > ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            Expr::GreaterThan(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) >= ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            Expr::GreaterThanEqual(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) < ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            Expr::LessThan(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) <= ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            Expr::LessThanEqual(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) & ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            Expr::And(lhs, rhs)
        }
    };
    ($a:ident, ($($lhs:tt)*) | ($($rhs:tt)*)) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            let rhs = expr!($a, $($rhs)*);
            let rhs = $a.append(rhs);
            Expr::Or(lhs, rhs)
        }
    };
    ($a:ident, !$($lhs:tt)*) => {
        {
            let lhs = expr!($a, $($lhs)*);
            let lhs = $a.append(lhs);
            Expr::Not(lhs)
        }
    };
}

#[cfg(test)]
mod test {
    use crate::ir::*;
    #[test]
    pub fn test() {
        let reg = Register(0);
        let mut arena: Arena<Expr<u32>> = Arena::new();

        let expr: Expr<u32> = expr!(arena,
            (load reg w) == (imm 1u8)
        );
        println!("{:?}", expr)
    }
}


pub(crate) use expr;