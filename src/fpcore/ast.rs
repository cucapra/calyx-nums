//! Abstract syntax for FPCore.

use calyx_utils::Id;
use std::fmt;

pub use super::constants::{MathConst, MathOp, TensorOp, TestOp};
pub use super::literals::{Rational, Sign};

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
pub enum ArgumentDef {
    Id(Symbol),
    Sized {
        var: Symbol,
        dims: Vec<Dimension>,
    },
    Annotated {
        props: Vec<Property>,
        var: Symbol,
        dims: Vec<Dimension>,
    },
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
pub enum Expression {
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

/// A numeric literal.
#[derive(Debug)]
pub enum Number {
    Rational(Rational),
    Digits {
        mantissa: Rational,
        exponent: Rational,
        base: Rational,
    },
}

#[derive(Debug)]
pub struct Property {
    pub name: Symbol,
    pub data: Data,
}

/// Property metadata.
#[derive(Debug)]
#[non_exhaustive]
pub enum Data {
    Str(String),
    Binder(Binder),
    Expr(Expression),
    List(Vec<Data>),
}

#[derive(Debug)]
pub struct Symbol(pub(super) Id);

#[derive(Debug)]
pub enum Operation {
    Math(MathOp),
    Test(TestOp),
    Tensor(TensorOp),
}

#[derive(Debug)]
pub enum Constant {
    Math(MathConst),
    Bool(bool),
}

impl Symbol {
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    pub fn as_id(&self) -> Id {
        self.0
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
