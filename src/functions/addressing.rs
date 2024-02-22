//! Lookup table addressing.

use std::fmt;

use calyx_utils as utils;

use crate::format::Format;
use crate::fpcore::ast::Rational;
use crate::utils::mangling::Mangle;

/// A specification for obtaining table indices from raw input values.
#[derive(Mangle)]
pub struct AddressSpec {
    pub subtrahend: u64,
    pub idx_lsb: u64,
    pub idx_width: u64,
}

impl AddressSpec {
    pub fn from_domain_hint(
        left: &Rational,
        right: &Rational,
        format: &Format,
        rows: u32,
    ) -> Result<(AddressSpec, TableDomain), DomainError> {
        let (domain, stride) =
            TableDomain::from_hint(left, right, format, rows)?;

        AddressSpec::validate_bounds(&domain, format)?;

        let subtrahend = domain.left.to_format(format).ok_or_else(|| {
            DomainError::new(
                DomainErrorKind::EndpointNotRepresentable,
                domain.left.clone(),
            )
        })?;

        let idx_lsb = u64::from(AddressSpec::validate_stride(stride, format)?);
        let idx_width = utils::bits_needed_for(u64::from(rows));

        Ok((
            AddressSpec {
                subtrahend,
                idx_lsb,
                idx_width,
            },
            domain,
        ))
    }

    fn validate_bounds(
        domain: &TableDomain,
        format: &Format,
    ) -> Result<(), DomainError> {
        let bound = Rational::power_of_two(
            format.msb() + 1 - i64::from(format.is_signed),
        );

        if domain.right > bound {
            return Err(DomainError::new(
                DomainErrorKind::EndpointOutOfBounds,
                domain.right.clone(),
            ));
        }

        let out_of_bounds = if format.is_signed {
            domain.left < -bound
        } else {
            domain.left.is_negative()
        };

        if out_of_bounds {
            Err(DomainError::new(
                DomainErrorKind::EndpointOutOfBounds,
                domain.left.clone(),
            ))
        } else {
            Ok(())
        }
    }

    /// Returns the exact base-2 log of the stride, or an error.
    fn validate_stride(
        stride: Rational,
        format: &Format,
    ) -> Result<u32, DomainError> {
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

/// An input domain for a lookup table.
#[derive(Mangle)]
pub struct TableDomain {
    pub left: Rational,
    pub right: Rational,
}

impl TableDomain {
    /// Attempts to widen the given domain so that it is amenable to
    /// implementation via a lookup table.
    fn from_hint(
        left: &Rational,
        right: &Rational,
        format: &Format,
        rows: u32,
    ) -> Result<(TableDomain, Rational), DomainError> {
        let a = left.floor(format.lsb());
        let b = right.ceil(format.lsb());

        let n = ((&b - &a) / Rational::from(rows)).ceil_log2();
        let stride = Rational::power_of_two(n);

        if n < format.lsb() {
            return Err(DomainError::new(
                DomainErrorKind::StrideNotRepresentable,
                stride,
            ));
        }

        let diameter = &stride * Rational::from(rows);

        let q = if a.sign != b.sign || a.is_zero() || b.is_zero() {
            &a * &diameter / (b - a)
        } else {
            &a * (&b + &b - &diameter) / (a + b)
        };

        let left = q.round_away(format.lsb());
        let right = &left + diameter;

        Ok((TableDomain { left, right }, stride))
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
