//! Built-in functions.

use crate::format::Format;
use crate::irgen::stdlib::{self, Primitive};

pub fn primitive_adder(format: &Format) -> &'static Primitive<'static> {
    match (format.frac_width, format.is_signed) {
        (0, false) => &stdlib::compile::STD_ADD,
        (0, true) => &stdlib::binary_operators::STD_SADD,
        (_, false) => &stdlib::binary_operators::STD_FP_ADD,
        (_, true) => &stdlib::binary_operators::STD_FP_SADD,
    }
}

pub fn primitive_subtractor(format: &Format) -> &'static Primitive<'static> {
    match (format.frac_width, format.is_signed) {
        (0, false) => &stdlib::core::STD_SUB,
        (0, true) => &stdlib::binary_operators::STD_SSUB,
        (_, false) => &stdlib::binary_operators::STD_FP_SUB,
        (_, true) => &stdlib::binary_operators::STD_FP_SSUB,
    }
}

pub fn primitive_multiplier(format: &Format) -> &'static Primitive<'static> {
    match (format.frac_width, format.is_signed) {
        (0, false) => &stdlib::binary_operators::STD_MULT_PIPE,
        (0, true) => &stdlib::binary_operators::STD_SMULT_PIPE,
        (_, false) => &stdlib::binary_operators::STD_FP_MULT_PIPE,
        (_, true) => &stdlib::binary_operators::STD_FP_SMULT_PIPE,
    }
}

pub fn primitive_divider(format: &Format) -> &'static Primitive<'static> {
    match (format.frac_width, format.is_signed) {
        (0, false) => &stdlib::binary_operators::STD_DIV_PIPE_QUOTIENT,
        (0, true) => &stdlib::binary_operators::STD_SDIV_PIPE_QUOTIENT,
        (_, false) => &stdlib::binary_operators::STD_FP_DIV_PIPE_QUOTIENT,
        (_, true) => &stdlib::binary_operators::STD_FP_SDIV_PIPE_QUOTIENT,
    }
}

pub fn primitive_sqrt(format: &Format) -> &'static Primitive<'static> {
    match format.frac_width {
        0 => &stdlib::math::STD_SQRT,
        _ => &stdlib::math::STD_FP_SQRT,
    }
}
