//! Name mangling.

use std::fmt;

use num::BigUint;

use super::sollya::SollyaFunction;
use crate::format::Format;
use crate::fpcore::ast::{Rational, Sign};
use crate::fpcore::metadata::CalyxImpl;
use crate::functions::TableDomain;

/// Encodes context information into an identifier. The resulting identifier is
/// a valid name in the IA-64 C++ ABI's name mangling scheme.
macro_rules! mangle {
    ($name:expr, $($arg:expr),+ $(,)?) => {{
        use $crate::utils::mangling::Mangle as _;

        let mut res = match $name {
            name => format!("_Z{}{}I", name.len(), name),
        };

        $(
            res.push('X');
            $arg.mangle(&mut res).unwrap();
            res.push('E');
        )+

        res.push('E');
        res
    }};
}

pub(crate) use mangle;

/// A trait for formatting data as a mangled expression.
pub trait Mangle {
    /// Formats `self` as an [`<expression>`].
    ///
    /// [`<expression>`]:
    ///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.expression
    fn mangle<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write;
}

/// Formats an [`<expression>`] given an [`<identifier>`] and a list of
/// [`<expression>`].
///
/// [`<expression>`]:
///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.expression
/// [`<identifier>`]:
///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.identifier
macro_rules! init_list {
    ($dst:ident, $type:literal, $($expr:expr),+ $(,)?) => {{
        write!($dst, "tl{}{}", $type.len(), $type)?;
        $(
            $expr.mangle($dst)?;
        )+
        write!($dst, "E")
    }};
}

impl Mangle for bool {
    fn mangle<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        write!(w, "Lb{}E", *self as u8)
    }
}

macro_rules! impl_mangle_unsigned {
    ($($type:ty),*) => {$(
        impl Mangle for $type {
            fn mangle<W>(&self, w: &mut W) -> fmt::Result
            where
                W: fmt::Write,
            {
                write!(w, "Lj{}E", self)
            }
        }
    )*};
}

impl_mangle_unsigned!(usize, u8, u16, u32, u64, BigUint);

macro_rules! impl_mangle_signed {
    ($($type:ty),*) => {$(
        impl Mangle for $type {
            fn mangle<W>(&self, w: &mut W) -> fmt::Result
            where
                W: fmt::Write,
            {
                if *self < 0 {
                    write!(w, "Lin{}E", self.unsigned_abs())
                } else {
                    write!(w, "Li{}E", self)
                }
            }
        }
    )*};
}

impl_mangle_signed!(isize, i8, i16, i32, i64);

impl Mangle for Format {
    fn mangle<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        init_list!(w, "Format", self.width, self.frac_width, self.is_signed)
    }
}

impl Mangle for Sign {
    fn mangle<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        write!(w, "L4Sign{}E", *self as u8)
    }
}

impl Mangle for Rational {
    fn mangle<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        init_list!(w, "Rational", self.sign, self.mag.numer(), self.mag.denom())
    }
}

impl Mangle for SollyaFunction {
    fn mangle<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        write!(w, "L14SollyaFunction{}E", *self as u8)
    }
}

impl Mangle for TableDomain {
    fn mangle<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        init_list!(w, "TableDomain", self.left, self.right)
    }
}

impl Mangle for CalyxImpl {
    fn mangle<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        write!(w, "tl9CalyxImpl")?;

        match self {
            CalyxImpl::Lut { lut_size } => {
                init_list!(w, "Lut", lut_size)?;
            }
            CalyxImpl::Poly { degree, lut_size } => {
                init_list!(w, "Poly", degree, lut_size)?;
            }
        };

        write!(w, "E")
    }
}
