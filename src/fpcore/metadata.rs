//! Metadata properties.

use std::str::FromStr;

use super::ast::{Binder, Expression, Number, Symbol};

#[derive(Debug)]
#[non_exhaustive]
pub enum Property {
    /// Benchmark name.
    Name(String),
    /// Benchmark description.
    Description(String),
    /// List of sources for the benchmark.
    Cite(Vec<Symbol>),
    /// Precision used for rounding.
    Precision(Precision),
    /// Rounding mode.
    Round(RoundingMode),
    /// Overflow behavior for fixed-point values.
    Overflow(OverflowMode),
    /// Precondition on benchmark inputs.
    Pre(Expression),
    /// Real-valued function approximated by the benchmark.
    Spec(Expression),
    /// Alternate formulation of the benchmark.
    Alt(Expression),
    /// Library to be used for mathematical operations.
    MathLib(Symbol),
    /// Example input.
    Example(Vec<Binder>),
    /// Unknown property.
    Unknown(Symbol, Data),
}

/// Property data.
#[derive(Debug)]
#[non_exhaustive]
pub enum Data {
    Symbol(Symbol),
    Num(Number),
    Str(String),
    List(Vec<Data>),
}

impl Property {
    pub fn name(&self) -> &str {
        match self {
            Property::Name(_) => "name",
            Property::Description(_) => "description",
            Property::Cite(_) => "cite",
            Property::Precision(_) => "precision",
            Property::Round(_) => "round",
            Property::Overflow(_) => "overflow",
            Property::Pre(_) => "pre",
            Property::Spec(_) => "spec",
            Property::Alt(_) => "alt",
            Property::MathLib(_) => "math-library",
            Property::Example(_) => "example",
            Property::Unknown(name, _) => name.as_str(),
        }
    }
}

/// A precision family and its parameters.
#[derive(Clone, Copy, Debug)]
pub enum Precision {
    Float { e: u32, nbits: u32 },
    Posit { es: u32, nbits: u32 },
    Fixed { scale: i32, nbits: u32 },
    Real,
    Integer,
}

impl Precision {
    pub fn from_shorthand(s: &str) -> Result<Self, ()> {
        match s {
            "binary16" => Ok(Precision::Float { e: 5, nbits: 16 }),
            "binary32" => Ok(Precision::Float { e: 8, nbits: 32 }),
            "binary64" => Ok(Precision::Float { e: 11, nbits: 64 }),
            "binary128" => Ok(Precision::Float { e: 15, nbits: 128 }),
            "binary256" => Ok(Precision::Float { e: 19, nbits: 256 }),
            "posit8" => Ok(Precision::Posit { es: 0, nbits: 8 }),
            "posit16" => Ok(Precision::Posit { es: 1, nbits: 16 }),
            "posit32" => Ok(Precision::Posit { es: 2, nbits: 32 }),
            "posit64" => Ok(Precision::Posit { es: 3, nbits: 64 }),
            "real" => Ok(Precision::Real),
            "integer" => Ok(Precision::Integer),
            _ => Err(()),
        }
    }
}

/// An IEEE 754 rounding mode.
#[derive(Clone, Copy, Debug)]
pub enum RoundingMode {
    NearestEven,
    NearestAway,
    ToPositive,
    ToNegative,
    ToZero,
}

impl FromStr for RoundingMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "nearestEven" => Ok(RoundingMode::NearestEven),
            "nearestAway" => Ok(RoundingMode::NearestAway),
            "toPositive" => Ok(RoundingMode::ToPositive),
            "toNegative" => Ok(RoundingMode::ToNegative),
            "toZero" => Ok(RoundingMode::ToZero),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum OverflowMode {
    Infinity,
    Clamp,
    Wrap,
}

impl FromStr for OverflowMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "infinity" => Ok(OverflowMode::Infinity),
            "clamp" => Ok(OverflowMode::Clamp),
            "wrap" => Ok(OverflowMode::Wrap),
            _ => Err(()),
        }
    }
}
