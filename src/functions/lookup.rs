//! Table lookup operations.

use calyx_ir::{self as ir, build_assignments, structure};
use calyx_utils::{CalyxResult, Error, Id};
use num::traits::{Pow, PrimInt, Unsigned};

use crate::format::Format;
use crate::fpcore::ast::Rational;
use crate::fpcore::metadata::CalyxDomain;

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
    domain: &CalyxDomain,
    lib: &ir::LibrarySignatures,
) -> CalyxResult<ir::Component> {
    let sup = supremum(format);

    if domain.right.rational > sup {
        return Err(Error::misc(format!(
            "Right endpoint {} exceeds maximum permissable value of {}",
            domain.right.rational, sup
        ))
        .with_pos(&domain.right));
    }

    let left = domain.left.rational.to_format(format).ok_or_else(|| {
        Error::misc(format!(
            "Left endpoint {} is not representable in the given format",
            domain.left.rational
        ))
        .with_pos(&domain.left)
    })?;

    let width = u64::from(cols) * format.width;
    let idx_width = u64::from(index_width(rows));
    let offset_width = u64::from(offset_width(rows, format, domain)?);

    let ports = signature(cols, format);

    let mut component = ir::Component::new(name, ports, true, None);
    let mut builder = ir::Builder::new(&mut component, lib);

    let prim = builder.add_primitive("lut", lut, &[width, idx_width]);

    structure!(builder;
        let sub = prim std_sub(format.width);
        let rsh = prim std_rsh(format.width);
        let slice = prim std_slice(format.width, idx_width);
        let left = constant(left, format.width);
        let shift = constant(offset_width, format.width);
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

/// For a format with resolution `r`, computes the greatest representable value
/// plus `r`.
fn supremum(format: &Format) -> Rational {
    let int_width = format.width - format.frac_width;

    let e = if format.is_signed {
        int_width as i64 - 1
    } else {
        int_width as i64
    };

    Rational::from(2u8).pow(e)
}

/// Computes the ceiling of the base-2 log of `size`.
///
/// In contrast to the formula `floor(log2(size - 1)) + 1`, this is defined for
/// `size == 1` and gives the answer `0`.
fn index_width<T: PrimInt + Unsigned>(size: T) -> u32 {
    let type_width = T::zero().count_zeros();

    type_width - T::leading_zeros(size - T::one())
}

/// Computes the position of the least significant bit of the index.
fn offset_width(
    rows: u32,
    format: &Format,
    domain: &CalyxDomain,
) -> CalyxResult<u32> {
    let stride = (domain.right.rational.clone() - domain.left.rational.clone())
        / Rational::from(rows);

    let stride_bits: u64 = stride.to_format(format).ok_or_else(|| {
        Error::misc(format!(
            "Stride {stride} is not representable in the given format"
        ))
    })?;

    if stride_bits.is_power_of_two() {
        Ok(stride_bits.trailing_zeros())
    } else {
        Err(Error::misc(format!(
            "Stride {stride} is not a power of two"
        )))
    }
}
