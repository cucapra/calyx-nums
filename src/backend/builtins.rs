//! Built-in functions.

use super::stdlib::{self, Primitive};
use crate::format::Format;

pub fn add(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::compile::STD_ADD,
        (0, true) => &stdlib::binary_operators::STD_SADD,
        (_, false) => &stdlib::binary_operators::STD_FP_ADD,
        (_, true) => &stdlib::binary_operators::STD_FP_SADD,
    }
}

pub fn sub(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::core::STD_SUB,
        (0, true) => &stdlib::binary_operators::STD_SSUB,
        (_, false) => &stdlib::binary_operators::STD_FP_SUB,
        (_, true) => &stdlib::binary_operators::STD_FP_SSUB,
    }
}

pub fn mul(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::binary_operators::STD_MULT_PIPE,
        (0, true) => &stdlib::binary_operators::STD_SMULT_PIPE,
        (_, false) => &stdlib::binary_operators::STD_FP_MULT_PIPE,
        (_, true) => &stdlib::binary_operators::STD_FP_SMULT_PIPE,
    }
}

pub fn div(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::binary_operators::STD_DIV_PIPE_QUOTIENT,
        (0, true) => &stdlib::binary_operators::STD_SDIV_PIPE_QUOTIENT,
        (_, false) => &stdlib::binary_operators::STD_FP_DIV_PIPE_QUOTIENT,
        (_, true) => &stdlib::binary_operators::STD_FP_SDIV_PIPE_QUOTIENT,
    }
}

pub fn gt(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::core::STD_GT,
        (0, true) => &stdlib::binary_operators::STD_SGT,
        (_, false) => &stdlib::binary_operators::STD_FP_GT,
        (_, true) => &stdlib::binary_operators::STD_FP_SGT,
    }
}

pub fn lt(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::core::STD_LT,
        (0, true) => &stdlib::binary_operators::STD_SLT,
        (_, false) => &stdlib::core::STD_LT, // missing STD_FP_LT
        (_, true) => &stdlib::binary_operators::STD_FP_SLT,
    }
}

pub fn eq(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::core::STD_EQ,
        (0, true) => &stdlib::binary_operators::STD_SEQ,
        (_, false) => &stdlib::core::STD_EQ, // missing STD_FP_EQ
        (_, true) => &stdlib::binary_operators::STD_SEQ, // missing STD_FP_SEQ
    }
}

pub fn neq(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::core::STD_NEQ,
        (0, true) => &stdlib::binary_operators::STD_SNEQ,
        (_, false) => &stdlib::core::STD_NEQ, // missing STD_FP_NEQ
        (_, true) => &stdlib::binary_operators::STD_SNEQ, // missing STD_FP_SNEQ
    }
}

pub fn ge(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::core::STD_GE,
        (0, true) => &stdlib::binary_operators::STD_SGE,
        (_, false) => &stdlib::core::STD_GE, // missing STD_FP_GE
        (_, true) => &stdlib::binary_operators::STD_SGE, // missing STD_FP_SGE
    }
}

pub fn le(format: &Format) -> &'static Primitive<'static> {
    match (format.scale, format.is_signed) {
        (0, false) => &stdlib::core::STD_LE,
        (0, true) => &stdlib::binary_operators::STD_SLE,
        (_, false) => &stdlib::core::STD_LE, // missing STD_FP_LE
        (_, true) => &stdlib::binary_operators::STD_SLE, // missing STD_FP_SLE
    }
}

pub fn sqrt(format: &Format) -> &'static Primitive<'static> {
    match format.scale {
        0 => &stdlib::math::STD_SQRT,
        _ => &stdlib::math::STD_FP_SQRT,
    }
}
