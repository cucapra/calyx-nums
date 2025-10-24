use std::fmt;

use super::Context;
use super::index as idx;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum SollyaExpr {
    Variable,
    Number(idx::NumIdx),
    Neg(idx::SollyaIdx),
    Binary(SollyaBinOp, idx::SollyaIdx, idx::SollyaIdx),
    Call(SollyaFn, idx::SollyaIdx),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SollyaBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl SollyaBinOp {
    pub fn as_str(self) -> &'static str {
        match self {
            SollyaBinOp::Add => "+",
            SollyaBinOp::Sub => "-",
            SollyaBinOp::Mul => "*",
            SollyaBinOp::Div => "/",
            SollyaBinOp::Pow => "^",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum SollyaFn {
    Sin,  Cos,   Tan,  Sinh,  Cosh,  Tanh,
    ASin, ACos,  ATan, ASinh, ACosh, ATanh,
    Exp,  ExpM1, Log,  Log2,  Log10, Log1P,
    Erf,  ErfC,  Sqrt,
}

impl SollyaFn {
    pub fn as_str(self) -> &'static str {
        match self {
            SollyaFn::Sin => "sin",
            SollyaFn::Cos => "cos",
            SollyaFn::Tan => "tan",
            SollyaFn::Sinh => "sinh",
            SollyaFn::Cosh => "cosh",
            SollyaFn::Tanh => "tanh",
            SollyaFn::ASin => "asin",
            SollyaFn::ACos => "acos",
            SollyaFn::ATan => "atan",
            SollyaFn::ASinh => "asinh",
            SollyaFn::ACosh => "acosh",
            SollyaFn::ATanh => "atanh",
            SollyaFn::Exp => "exp",
            SollyaFn::ExpM1 => "expm1",
            SollyaFn::Log => "log",
            SollyaFn::Log2 => "log2",
            SollyaFn::Log10 => "log10",
            SollyaFn::Log1P => "log1p",
            SollyaFn::Erf => "erf",
            SollyaFn::ErfC => "erfc",
            SollyaFn::Sqrt => "sqrt",
        }
    }
}

impl idx::SollyaIdx {
    /// If `self` represents a named function, get its name as an identifier.
    pub fn name(self, ctx: &Context) -> Option<&'static str> {
        if let SollyaExpr::Call(f, arg) = ctx[self]
            && let SollyaExpr::Variable = ctx[arg]
        {
            Some(f.as_str())
        } else {
            None
        }
    }

    /// Returns an adapter for formatting `self` as a Sollya expression in the
    /// free variable.
    pub fn sollya(self, ctx: &Context) -> impl fmt::Display {
        Printer { ctx, idx: self }
    }
}

struct Printer<'ctx> {
    ctx: &'ctx Context,
    idx: idx::SollyaIdx,
}

impl fmt::Display for Printer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut stack = vec![(self.idx, 0u32)];

        while let Some((idx, state)) = stack.pop() {
            match self.ctx[idx] {
                SollyaExpr::Variable => write!(f, "_x_")?,
                SollyaExpr::Number(num) => {
                    write!(f, "({})", self.ctx[num].value)?;
                }
                SollyaExpr::Neg(arg) => {
                    write!(f, "-")?;

                    stack.push((arg, 0));
                }
                SollyaExpr::Binary(op, lhs, rhs) => {
                    if state == 0 {
                        write!(f, "(")?;

                        stack.push((idx, state + 1));
                        stack.push((lhs, 0));
                    } else if state == 1 {
                        write!(f, "{}", op.as_str())?;

                        stack.push((idx, state + 1));
                        stack.push((rhs, 0));
                    } else {
                        write!(f, ")")?;
                    }
                }
                SollyaExpr::Call(op, arg) => {
                    if state == 0 {
                        write!(f, "{}(", op.as_str())?;

                        stack.push((idx, state + 1));
                        stack.push((arg, 0));
                    } else {
                        write!(f, ")")?;
                    }
                }
            }
        }

        Ok(())
    }
}
