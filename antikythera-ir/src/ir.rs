use crate::arena::Handle;

pub trait AddressSize: bincode::Encode + bincode::Decode + 'static {}

impl AddressSize for u8 {}
impl AddressSize for u16 {}
impl AddressSize for u32 {}
impl AddressSize for u64 {}

/// An immediate value.
#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum ImmediateValue {
    One(u8),
    Two(u16),
    Four(u32),
    Eight(u64),
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Location<A: AddressSize> {
    Register(u8),
    Address(A),
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Operand<A: AddressSize> {
    Location(Location<A>),
    Immediate(ImmediateValue),
}

#[derive(Eq, PartialEq, Hash, bincode::Encode, bincode::Decode)]
pub enum Expression<A: AddressSize> {
    Equal(Handle<Expression<A>>, Handle<Expression<A>>),
    GreaterThan(Handle<Expression<A>>, Handle<Expression<A>>),
    GreaterThanEqual(Handle<Expression<A>>, Handle<Expression<A>>),
    LessThan(Handle<Expression<A>>, Handle<Expression<A>>),
    LessThanEqual(Handle<Expression<A>>, Handle<Expression<A>>),
    And(Handle<Expression<A>>, Handle<Expression<A>>),
    Or(Handle<Expression<A>>, Handle<Expression<A>>),
    Not(Handle<Expression<A>>),
    Literal(Operand<A>),
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
        register: u8,
    },
    /// Increment the given register. On overflow, the register will wrap.
    Increment {
        value: A,
        register: u8,
    },
    /// Decrement the given register. On underflow, the register will wrap.
    Decrement {
        value: A,
        register: u8,
    },
    /// Load the value at the guest address into the register.
    LoadMemory {
        source: A,
        register: u8,
    },
    /// Write the value stored in the register into the location `dest`
    Store {
        dest: Location<A>,
        register: u8,
    },

    // Memory operations
    /// Write an immediate value to the destination.
    ///
    /// Cheat operations involving 8, 16, 32, and 64 bit immediate writes
    /// desugar to this operation.
    WriteImmediate {
        value: ImmediateValue,
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
        value: ImmediateValue,
        dest: Location<A>,
        length: u64,
        offset: A,
    },

    // Branch to the block if the expression evaluates to true.
    Branch {
        cond: Option<Handle<Expression<A>>>,
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
