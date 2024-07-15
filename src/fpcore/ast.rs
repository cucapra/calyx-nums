//! Abstract syntax for FPCore.

use std::sync::atomic::{AtomicU32, Ordering};

use calyx_utils::{GPosIdx, Id, WithPos};

pub use super::constants::{MathConst, MathOp, TensorOp, TestOp};
pub use super::literals::Rational;
pub use super::metadata::Property;

#[derive(Debug)]
pub struct FPCore {
    /// Operator name.
    pub name: Option<Symbol>,
    /// List of free variables.
    pub args: Vec<Argument>,
    /// List of metadata properties.
    pub props: Vec<Property>,
    /// Operator body.
    pub body: Expression,
}

#[derive(Debug)]
pub enum Dimension {
    /// Variable to bind to dimension size.
    Id(Symbol),
    /// Fixed dimension size.
    Num(Number),
}

/// A formal parameter.
#[derive(Debug)]
pub struct Argument {
    pub var: Symbol,
    pub props: Vec<Property>,
    pub dims: Vec<Dimension>,
    pub uid: NodeId,
}

#[derive(Debug)]
pub struct Binding {
    pub var: Symbol,
    pub expr: Expression,
}

#[derive(Debug)]
pub struct MutableVar {
    /// Variable name.
    pub var: Symbol,
    /// Initial value.
    pub init: Expression,
    /// Expression producing an updated value.
    pub update: Expression,
}

#[derive(Debug)]
pub struct InductionVar {
    /// Variable name.
    pub var: Symbol,
    /// Number of iterations.
    pub size: Expression,
}

#[derive(Debug)]
pub enum ExprKind {
    Num(Number),
    Const(Constant),
    Id(Symbol),
    Op(Operation, Vec<Expression>),
    If {
        cond: Box<Expression>,
        true_branch: Box<Expression>,
        false_branch: Box<Expression>,
    },
    Let {
        bindings: Vec<Binding>,
        body: Box<Expression>,
        sequential: bool,
    },
    While {
        cond: Box<Expression>,
        vars: Vec<MutableVar>,
        body: Box<Expression>,
        sequential: bool,
    },
    For {
        indices: Vec<InductionVar>,
        vars: Vec<MutableVar>,
        body: Box<Expression>,
        sequential: bool,
    },
    Tensor {
        indices: Vec<InductionVar>,
        body: Box<Expression>,
    },
    TensorStar {
        indices: Vec<InductionVar>,
        vars: Vec<MutableVar>,
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
    pub span: Span,
}

/// A numeric literal.
#[derive(Debug)]
pub struct Number {
    pub value: Rational,
    pub span: Span,
}

#[derive(Debug)]
pub struct Symbol {
    pub id: Id,
    pub span: Span,
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
    pub span: Span,
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

/// A region of source code.
#[derive(Clone, Copy, Debug)]
pub struct Span(pub(super) GPosIdx);

impl WithPos for Span {
    fn copy_span(&self) -> GPosIdx {
        self.0
    }
}

impl WithPos for Expression {
    fn copy_span(&self) -> GPosIdx {
        self.span.0
    }
}

impl WithPos for Number {
    fn copy_span(&self) -> GPosIdx {
        self.span.0
    }
}

impl WithPos for Symbol {
    fn copy_span(&self) -> GPosIdx {
        self.span.0
    }
}

impl WithPos for Operation {
    fn copy_span(&self) -> GPosIdx {
        self.span.0
    }
}
