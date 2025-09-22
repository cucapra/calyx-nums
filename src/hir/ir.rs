use super::arena::{EntityList, PackedOption};
use super::index as idx;
use crate::fpcore::metadata as meta;

pub use crate::fpcore::ast::{
    Constant, Id, MathConst, MathOp, Number, Rational, Span, Symbol, TestOp,
};

pub struct Definition {
    pub name: Option<Symbol>,
    pub args: idx::IndexRange<idx::ArgIdx>,
    pub scope: PackedOption<idx::ScopeIdx>,
    pub body: idx::ExprIdx,
}

pub struct Argument {
    pub var: Symbol,
    pub scope: PackedOption<idx::ScopeIdx>,
}

pub enum ExprKind {
    Num(idx::NumIdx),
    Const(Constant),
    Var(idx::VarIdx, VarKind),
    Op(Operation, EntityList<idx::ExprIdx>),
    If(If),
    Let(Let),
    While(While),
}

pub struct Expression {
    pub kind: ExprKind,
    pub scope: PackedOption<idx::ScopeIdx>,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub enum VarKind {
    Arg(idx::ArgIdx),
    Let(idx::ExprIdx),
    Mut,
}

#[derive(Clone, Copy)]
pub enum OpKind {
    Math(MathOp),
    Test(TestOp),
    Def(idx::DefIdx),
}

pub struct Operation {
    pub kind: OpKind,
    pub span: Span,
}

pub struct If {
    pub cond: idx::ExprIdx,
    pub if_true: idx::ExprIdx,
    pub if_false: idx::ExprIdx,
}

pub struct Let {
    pub writes: EntityList<idx::WriteIdx>,
    pub body: idx::ExprIdx,
    pub sequential: bool,
}

pub struct While {
    pub cond: idx::ExprIdx,
    pub inits: EntityList<idx::WriteIdx>,
    pub updates: EntityList<idx::WriteIdx>,
    pub body: idx::ExprIdx,
    pub sequential: bool,
}

pub struct Write {
    pub var: idx::VarIdx,
    pub val: idx::ExprIdx,
}

pub struct Scope {
    pub prop: Property,
    pub parent: PackedOption<idx::ScopeIdx>,
}

pub enum Property {
    Pre(idx::ExprIdx),
    Domain(Domain),
    Impl(Strategy),
}

#[derive(Clone, Copy)]
pub struct Domain {
    pub left: idx::NumIdx,
    pub right: idx::NumIdx,
}

pub type Strategy = meta::CalyxImpl;
