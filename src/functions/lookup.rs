//! Table lookup operations.

use std::fmt;

use calyx_ir::{self as ir, build_assignments, structure};
use calyx_utils::{self as utils, Id};

use crate::format::Format;
use crate::fpcore::ast::Rational;
use crate::fpcore::metadata::CalyxImpl;

/// Returns the signature for a table-lookup component.
pub fn signature(cols: u32, format: &Format) -> Vec<ir::PortDef<u64>> {
    vec![
        ir::PortDef {
            name: Id::new("in"),
            width: format.width,
            direction: ir::Direction::Input,
            attributes: Default::default(),
        },
        ir::PortDef {
            name: Id::new("out"),
            width: u64::from(cols) * format.width,
            direction: ir::Direction::Output,
            attributes: Default::default(),
        },
    ]
}

pub fn compile_lookup(
    name: Id,
    lut: Id,
    rows: u32,
    cols: u32,
    format: &Format,
    domain: &TableDomain,
    lib: &ir::LibrarySignatures,
) -> Result<ir::Component, DomainError> {
    domain.validate_bounds(format)?;

    let left = domain.left.to_format(format).ok_or_else(|| {
        DomainError::new(
            DomainErrorKind::EndpointNotRepresentable,
            domain.left.clone(),
        )
    })?;

    let width = u64::from(cols) * format.width;
    let idx_width = utils::bits_needed_for(rows.into());
    let idx_lsb = u64::from(domain.checked_stride(rows, format)?);

    let ports = signature(cols, format);

    let mut component = ir::Component::new(name, ports, false, true, None);
    let mut builder = ir::Builder::new(&mut component, lib);

    let prim = builder.add_primitive("lut", lut, &[width, idx_width]);

    structure!(builder;
        let sub = prim std_sub(format.width);
        let rsh = prim std_rsh(format.width);
        let slice = prim std_slice(format.width, idx_width);
        let left = constant(left, format.width);
        let shift = constant(idx_lsb, format.width);
    );

    let signature = &builder.component.signature;

    let assigns = Vec::from(build_assignments!(builder;
        sub["left"] = ? signature["in"];
        sub["right"] = ? left["out"];
        rsh["left"] = ? sub["out"];
        rsh["right"] = ? shift["out"];
        slice["in"] = ? rsh["out"];
        prim["idx"] = ? slice["out"];
        signature["out"] = ? prim["out"];
    ));

    builder.add_continuous_assignments(assigns);

    Ok(component)
}

/// An input domain for a lookup table.
pub struct TableDomain {
    pub left: Rational,
    pub right: Rational,
}

impl TableDomain {
    /// Attempts to widen the given domain so that it is amenable to
    /// implementation via a lookup table.
    pub fn from_hint(
        left: &Rational,
        right: &Rational,
        strategy: &CalyxImpl,
        format: &Format,
    ) -> TableDomain {
        let rows = match *strategy {
            CalyxImpl::Lut { lut_size } => lut_size,
            CalyxImpl::Poly { lut_size, .. } => lut_size,
        };

        let a = left.floor(format.frac_width);
        let b = right.ceil(format.frac_width);

        let n = ((&b - &a) / Rational::from(rows)).ceil_log2();

        if n.is_negative() && n.unsigned_abs() > format.frac_width {
            return TableDomain { left: a, right: b }; // Give up
        }

        let diameter = Rational::power_of_two(n) * Rational::from(rows);

        let q = if a.sign != b.sign || a.is_zero() || b.is_zero() {
            &a * &diameter / (b - a)
        } else {
            &a * (&b + &b - &diameter) / (a + b)
        };

        let left = q.round_away(format.frac_width);
        let right = &left + diameter;

        TableDomain { left, right }
    }

    fn validate_bounds(&self, format: &Format) -> Result<(), DomainError> {
        let int_width = format.width - format.frac_width;

        let bound =
            Rational::power_of_two(int_width as i64 - format.is_signed as i64);

        if self.right > bound {
            return Err(DomainError::new(
                DomainErrorKind::EndpointOutOfBounds,
                self.right.clone(),
            ));
        }

        let out_of_bounds = if format.is_signed {
            self.left < -bound
        } else {
            self.left.is_negative()
        };

        if out_of_bounds {
            Err(DomainError::new(
                DomainErrorKind::EndpointOutOfBounds,
                self.left.clone(),
            ))
        } else {
            Ok(())
        }
    }

    /// Returns the exact base-2 log of the stride, or an error.
    fn checked_stride(
        &self,
        rows: u32,
        format: &Format,
    ) -> Result<u32, DomainError> {
        let stride = (&self.right - &self.left) / Rational::from(rows);

        let Some(stride_bits) = stride.to_format::<u64>(format) else {
            return Err(DomainError::new(
                DomainErrorKind::StrideNotRepresentable,
                stride,
            ));
        };

        if stride_bits.is_power_of_two() {
            Ok(stride_bits.trailing_zeros())
        } else {
            Err(DomainError::new(
                DomainErrorKind::StrideNotPowerOfTwo,
                stride,
            ))
        }
    }
}

#[derive(Debug)]
pub enum DomainErrorKind {
    EndpointNotRepresentable,
    EndpointOutOfBounds,
    StrideNotRepresentable,
    StrideNotPowerOfTwo,
}

#[derive(Debug)]
pub struct DomainError {
    pub kind: DomainErrorKind,
    pub source: Rational,
}

impl DomainError {
    fn new(kind: DomainErrorKind, source: Rational) -> DomainError {
        DomainError { kind, source }
    }
}

impl fmt::Display for DomainError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            DomainErrorKind::EndpointNotRepresentable => {
                write!(f, "endpoint {} is not representable", self.source)
            }
            DomainErrorKind::EndpointOutOfBounds => {
                write!(f, "endpoint {} is out of bounds", self.source)
            }
            DomainErrorKind::StrideNotRepresentable => {
                write!(f, "stride {} is not representable", self.source)
            }
            DomainErrorKind::StrideNotPowerOfTwo => {
                write!(f, "stride {} is not a power of two", self.source)
            }
        }
    }
}
