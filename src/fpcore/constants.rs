//! Built-in operations and constants.

use std::str::FromStr;

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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
