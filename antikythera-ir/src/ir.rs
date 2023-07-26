use crate::arena::{Arena, Handle};

pub trait AddressSize: bincode::Encode + bincode::Decode + 'static {}

impl AddressSize for u8 {}
impl AddressSize for u16 {}
impl AddressSize for u32 {}
impl AddressSize for u64 {}

/// An untyped immediate value.
#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Immediate {
    One(u8),
    Two(u16),
    Four(u32),
    Eight(u64),
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
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

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Location<A: AddressSize> {
    /// Load the location from the given register.
    Register(Register),
    /// Use the statically known address.
    Address(A),
    /// Use the location defined as the given register + offset
    Offset(Register, A),
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Operand<A: AddressSize> {
    Load(Location<A>, Width),
    Immediate(Immediate),
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum BoolExpr<A: AddressSize> {
    Equal(Handle<BoolExpr<A>>, Handle<BoolExpr<A>>),
    GreaterThan(Handle<BoolExpr<A>>, Handle<BoolExpr<A>>),
    GreaterThanEqual(Handle<BoolExpr<A>>, Handle<BoolExpr<A>>),
    LessThan(Handle<BoolExpr<A>>, Handle<BoolExpr<A>>),
    LessThanEqual(Handle<BoolExpr<A>>, Handle<BoolExpr<A>>),
    And(Handle<BoolExpr<A>>, Handle<BoolExpr<A>>),
    Or(Handle<BoolExpr<A>>, Handle<BoolExpr<A>>),
    Not(Handle<BoolExpr<A>>),
    Literal(Operand<A>),
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode, Clone, Copy)]
pub struct Register(u8);

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Overflow {
    Saturating,
    Wrapping,
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum MathOp {
    Add,
    Sub,
    Mul,
    Div,
    Shl,
    Shr,
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum NumType {
    Integer,
    Floating,
}

/// Represents an operation on the antikythera virtual machine.
///
/// - A cheat with address size `A` has access to 256 persistent
///   registers (`u8::MAX`) of width `A`
#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
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
        cond: Option<Handle<BoolExpr<A>>>,
        target: Handle<Block<A>>,
    },

    Return,
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub struct Block<A: AddressSize>(Vec<Op<A>>);
impl<A: AddressSize> Block<A> {
    pub fn new(instrs: impl IntoIterator<Item = Op<A>>) -> Self {
        Block(instrs.into_iter().collect())
    }
}

pub struct Program<A: AddressSize> {
    pub entry: Handle<Block<A>>,
    pub blocks: Arena<Block<A>>,
    pub exprs: Arena<BoolExpr<A>>,
}

pub struct ProgramBuilder<A: AddressSize> {
    pub blocks: Arena<Block<A>>,
    pub exprs: Arena<BoolExpr<A>>,
}

impl<A: AddressSize> ProgramBuilder<A> {
    pub fn new() -> Self {
        let exprs = Arena::new();
        let mut blocks = Arena::new();
        Self { blocks, exprs }
    }
}
