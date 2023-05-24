//! Abstract syntax for FPCore.

pub use super::constants::{MathConst, MathOp, TensorOp, TestOp};

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
    Dec(DecNum),
    Hex(HexNum),
    Digits(Digits),
}

#[derive(Debug)]
pub struct Rational {
    pub negative: bool,
    /// Decimal digits of the numerator.
    pub numerator: String,
    /// Decimal digits of the denominator (not all zero).
    pub denominator: String,
}

#[derive(Debug)]
pub struct Exponent {
    pub negative: bool,
    /// Decimal digits of the exponent.
    pub exponent: String,
}

#[derive(Debug)]
pub struct DecNum {
    pub negative: bool,
    /// Decimal digits of the integer part.
    pub integer: String,
    /// Decimal digits of the fractional part.
    pub fraction: String,
    /// Exponent of an optional multiplier (with an implicit base of 10).
    pub exponent: Option<Exponent>,
}

#[derive(Debug)]
pub struct HexNum {
    pub negative: bool,
    /// Hexadecimal digits of the integer part.
    pub integer: String,
    /// Hexadecimal digits of the fractional part.
    pub fraction: String,
    /// Exponent of an optional multiplier (with an implicit base of 2).
    pub exponent: Option<Exponent>,
}

#[derive(Debug)]
pub struct Digits {
    pub mantissa: DecNum,
    pub exponent: DecNum,
    pub base: DecNum,
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
pub struct Symbol(pub String);

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
