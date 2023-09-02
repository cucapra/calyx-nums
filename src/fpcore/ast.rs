//! Abstract syntax for FPCore.

use std::sync::atomic::{AtomicU32, Ordering};

use calyx_utils::{GPosIdx, Id, WithPos};

pub use super::constants::{MathConst, MathOp, TensorOp, TestOp};
pub use super::literals::{Rational, Sign};
pub use super::metadata::Property;

/// An FPCore benchmark.
#[derive(Debug)]
pub struct BenchmarkDef {
    /// Benchmark name.
    pub name: Option<Symbol>,
    /// List of free variables.
    pub args: Vec<ArgumentDef>,
    /// List of metadata properties.
    pub props: Vec<Property>,
    /// Benchmark expression.
    pub body: Expression,
}

#[derive(Debug)]
pub enum Dimension {
    Id(Symbol),
    Num(Number),
}

/// A formal parameter to a benchmark.
#[derive(Debug)]
pub struct ArgumentDef {
    pub var: Symbol,
    pub props: Vec<Property>,
    pub dims: Vec<Dimension>,
    pub uid: NodeId,
}

#[derive(Debug)]
pub struct Binder {
    pub var: Symbol,
    pub expr: Expression,
}

#[derive(Debug)]
pub struct UpdateRule {
    /// Variable name.
    pub var: Symbol,
    /// Initial value.
    pub init: Expression,
    /// Expression producing an updated value.
    pub update: Expression,
}

/// A "less-than" condition.
#[derive(Debug)]
pub struct Condition {
    /// Variable name.
    pub var: Symbol,
    /// Value to compare to.
    pub val: Expression,
}

#[derive(Debug)]
pub enum ExprKind {
    Num(Number),
    Const(Constant),
    Id(Symbol),
    Op(Operation, Vec<Expression>),
    If {
        cond: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
    Let {
        binders: Vec<Binder>,
        body: Box<Expression>,
        sequential: bool,
    },
    While {
        cond: Box<Expression>,
        rules: Vec<UpdateRule>,
        body: Box<Expression>,
        sequential: bool,
    },
    For {
        conditions: Vec<Condition>,
        rules: Vec<UpdateRule>,
        body: Box<Expression>,
        sequential: bool,
    },
    Tensor {
        conditions: Vec<Condition>,
        body: Box<Expression>,
    },
    TensorStar {
        conditions: Vec<Condition>,
        rules: Vec<UpdateRule>,
        body: Box<Expression>,
    },
    Cast(Box<Expression>),
    Array(Vec<Expression>),
    Annotation {
        props: Vec<Property>,
        body: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExprKind,
    pub uid: NodeId,
    pub span: GPosIdx,
}

/// A numeric literal.
#[derive(Debug)]
pub struct Number {
    pub value: Rational,
    pub span: GPosIdx,
}

#[derive(Debug)]
pub struct Symbol {
    pub id: Id,
    pub span: GPosIdx,
}

#[derive(Clone, Copy, Debug)]
pub enum OpKind {
    Math(MathOp),
    Test(TestOp),
    Tensor(TensorOp),
}

#[derive(Debug)]
pub struct Operation {
    pub kind: OpKind,
    pub span: GPosIdx,
}

#[derive(Clone, Copy, Debug)]
pub enum Constant {
    Math(MathConst),
    Bool(bool),
}

/// Uniquely identifies an AST node.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub(crate) u32);

impl NodeId {
    /// Creates a new identifier that is distinct from all previously created
    /// identifiers.
    #[allow(clippy::new_without_default)]
    pub fn new() -> NodeId {
        static NEXT: AtomicU32 = AtomicU32::new(0);

        NodeId(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

impl WithPos for Expression {
    fn copy_span(&self) -> GPosIdx {
        self.span
    }
}

impl WithPos for Number {
    fn copy_span(&self) -> GPosIdx {
        self.span
    }
}

impl WithPos for Symbol {
    fn copy_span(&self) -> GPosIdx {
        self.span
    }
}

impl WithPos for Operation {
    fn copy_span(&self) -> GPosIdx {
        self.span
    }
}
