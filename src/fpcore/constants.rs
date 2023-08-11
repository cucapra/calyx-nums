//! Built-in operations and constants.

use std::str::FromStr;

#[derive(Clone, Copy, Debug)]
#[rustfmt::skip]
pub enum MathOp {
    Add,   Sub,      Mul,       Div,    FAbs,
    FMA,   Exp,      Exp2,      ExpM1,  Log,
    Log10, Log2,     Log1P,     Pow,    Sqrt,
    Cbrt,  Hypot,    Sin,       Cos,    Tan,
    ASin,  ACos,     ATan,      ATan2,  Sinh,
    Cosh,  Tanh,     ASinh,     ACosh,  ATanh,
    Erf,   ErfC,     TGamma,    LGamma, Ceil,
    Floor, FMod,     Remainder, FMax,   FMin,
    FDim,  CopySign, Trunc,     Round,  NearbyInt,
}

impl FromStr for MathOp {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(MathOp::Add),
            "-" => Ok(MathOp::Sub),
            "*" => Ok(MathOp::Mul),
            "/" => Ok(MathOp::Div),
            "fabs" => Ok(MathOp::FAbs),
            "fma" => Ok(MathOp::FMA),
            "exp" => Ok(MathOp::Exp),
            "exp2" => Ok(MathOp::Exp2),
            "expm1" => Ok(MathOp::ExpM1),
            "log" => Ok(MathOp::Log),
            "log10" => Ok(MathOp::Log10),
            "log2" => Ok(MathOp::Log2),
            "log1p" => Ok(MathOp::Log1P),
            "pow" => Ok(MathOp::Pow),
            "sqrt" => Ok(MathOp::Sqrt),
            "cbrt" => Ok(MathOp::Cbrt),
            "hypot" => Ok(MathOp::Hypot),
            "sin" => Ok(MathOp::Sin),
            "cos" => Ok(MathOp::Cos),
            "tan" => Ok(MathOp::Tan),
            "asin" => Ok(MathOp::ASin),
            "acos" => Ok(MathOp::ACos),
            "atan" => Ok(MathOp::ATan),
            "atan2" => Ok(MathOp::ATan2),
            "sinh" => Ok(MathOp::Sinh),
            "cosh" => Ok(MathOp::Cosh),
            "tanh" => Ok(MathOp::Tanh),
            "asinh" => Ok(MathOp::ASinh),
            "acosh" => Ok(MathOp::ACosh),
            "atanh" => Ok(MathOp::ATanh),
            "erf" => Ok(MathOp::Erf),
            "erfc" => Ok(MathOp::ErfC),
            "tgamma" => Ok(MathOp::TGamma),
            "lgamma" => Ok(MathOp::LGamma),
            "ceil" => Ok(MathOp::Ceil),
            "floor" => Ok(MathOp::Floor),
            "fmod" => Ok(MathOp::FMod),
            "remainder" => Ok(MathOp::Remainder),
            "fmax" => Ok(MathOp::FMax),
            "fmin" => Ok(MathOp::FMin),
            "fdim" => Ok(MathOp::FDim),
            "copysign" => Ok(MathOp::CopySign),
            "trunc" => Ok(MathOp::Trunc),
            "round" => Ok(MathOp::Round),
            "nearbyint" => Ok(MathOp::NearbyInt),
            _ => Err(()),
        }
    }
}

impl MathOp {
    pub fn arity(&self) -> usize {
        match self {
            MathOp::Add => 2,
            MathOp::Sub => 2,
            MathOp::Mul => 2,
            MathOp::Div => 2,
            MathOp::FAbs => 1,
            MathOp::FMA => 3,
            MathOp::Exp => 1,
            MathOp::Exp2 => 1,
            MathOp::ExpM1 => 1,
            MathOp::Log => 1,
            MathOp::Log10 => 1,
            MathOp::Log2 => 1,
            MathOp::Log1P => 1,
            MathOp::Pow => 2,
            MathOp::Sqrt => 1,
            MathOp::Cbrt => 1,
            MathOp::Hypot => 2,
            MathOp::Sin => 1,
            MathOp::Cos => 1,
            MathOp::Tan => 1,
            MathOp::ASin => 1,
            MathOp::ACos => 1,
            MathOp::ATan => 1,
            MathOp::ATan2 => 2,
            MathOp::Sinh => 1,
            MathOp::Cosh => 1,
            MathOp::Tanh => 1,
            MathOp::ASinh => 1,
            MathOp::ACosh => 1,
            MathOp::ATanh => 1,
            MathOp::Erf => 1,
            MathOp::ErfC => 1,
            MathOp::TGamma => 1,
            MathOp::LGamma => 1,
            MathOp::Ceil => 1,
            MathOp::Floor => 1,
            MathOp::FMod => 2,
            MathOp::Remainder => 2,
            MathOp::FMax => 2,
            MathOp::FMin => 2,
            MathOp::FDim => 2,
            MathOp::CopySign => 2,
            MathOp::Trunc => 1,
            MathOp::Round => 1,
            MathOp::NearbyInt => 1,
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[rustfmt::skip]
pub enum TestOp {
    Lt,    Gt,    Leq,      Geq,     Eq,
    Neq,   And,   Or,       Not,     IsFinite,
    IsInf, IsNan, IsNormal, SignBit,
}

impl FromStr for TestOp {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "<" => Ok(TestOp::Lt),
            ">" => Ok(TestOp::Gt),
            "<=" => Ok(TestOp::Leq),
            ">=" => Ok(TestOp::Geq),
            "==" => Ok(TestOp::Eq),
            "!=" => Ok(TestOp::Neq),
            "and" => Ok(TestOp::And),
            "or" => Ok(TestOp::Or),
            "not" => Ok(TestOp::Not),
            "isfinite" => Ok(TestOp::IsFinite),
            "isinf" => Ok(TestOp::IsInf),
            "isnan" => Ok(TestOp::IsNan),
            "isnormal" => Ok(TestOp::IsNormal),
            "signbit" => Ok(TestOp::SignBit),
            _ => Err(()),
        }
    }
}

impl TestOp {
    pub fn arity(&self) -> usize {
        match self {
            TestOp::Lt => 2,
            TestOp::Gt => 2,
            TestOp::Leq => 2,
            TestOp::Geq => 2,
            TestOp::Eq => 2,
            TestOp::Neq => 2,
            TestOp::And => 2,
            TestOp::Or => 2,
            TestOp::Not => 1,
            TestOp::IsFinite => 1,
            TestOp::IsInf => 1,
            TestOp::IsNan => 1,
            TestOp::IsNormal => 1,
            TestOp::SignBit => 1,
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[rustfmt::skip]
pub enum TensorOp {
    Dim, Size, Ref,
}

impl FromStr for TensorOp {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "dim" => Ok(TensorOp::Dim),
            "size" => Ok(TensorOp::Size),
            "ref" => Ok(TensorOp::Ref),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[allow(non_camel_case_types)]
#[rustfmt::skip]
pub enum MathConst {
    E,          LOG2E, LOG10E,  LN2,      LN10,
    PI,         PI_2,  PI_4,    M_1_PI,   M_2_PI,
    M_2_SQRTPI, SQRT2, SQRT1_2, INFINITY, NAN,
}

impl FromStr for MathConst {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "E" => Ok(MathConst::E),
            "LOG2E" => Ok(MathConst::LOG2E),
            "LOG10E" => Ok(MathConst::LOG10E),
            "LN2" => Ok(MathConst::LN2),
            "LN10" => Ok(MathConst::LN10),
            "PI" => Ok(MathConst::PI),
            "PI_2" => Ok(MathConst::PI_2),
            "PI_4" => Ok(MathConst::PI_4),
            "M_1_PI" => Ok(MathConst::M_1_PI),
            "M_2_PI" => Ok(MathConst::M_2_PI),
            "M_2_SQRTPI" => Ok(MathConst::M_2_SQRTPI),
            "SQRT2" => Ok(MathConst::SQRT2),
            "SQRT1_2" => Ok(MathConst::SQRT1_2),
            "INFINITY" => Ok(MathConst::INFINITY),
            "NAN" => Ok(MathConst::NAN),
            _ => Err(()),
        }
    }
}
