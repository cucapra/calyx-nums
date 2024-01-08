//! Standard library primitive declarations.

use smallvec::{smallvec, SmallVec};

use crate::format::Format;

pub enum Parameters {
    Bitnum,
    FixedPoint,
}

pub enum Arguments<'a> {
    Unary { input: &'a str },
    Binary { left: &'a str, right: &'a str },
}

impl Arguments<'_> {
    pub const UNARY_DEFAULT: Self = Self::Unary { input: "in" };

    pub const BINARY_DEFAULT: Self = Self::Binary {
        left: "left",
        right: "right",
    };
}

pub struct Signature<'a> {
    pub args: Arguments<'a>,
    pub output: &'a str,
}

impl Signature<'_> {
    pub const UNARY_DEFAULT: Self = Self {
        args: Arguments::UNARY_DEFAULT,
        output: "out",
    };

    pub const BINARY_DEFAULT: Self = Self {
        args: Arguments::BINARY_DEFAULT,
        output: "out",
    };
}

pub struct Primitive<'a> {
    pub name: &'a str,
    pub prefix_hint: &'a str,
    pub signature: Signature<'a>,
    pub params: Parameters,
    pub is_comb: bool,
}

impl Primitive<'_> {
    /// Constructs a parameter vector to instantiate the primitive for the given
    /// format.
    ///
    /// # Panics
    ///
    /// May panic if the primitive does not support the format.
    pub fn build_params(&self, format: &Format) -> SmallVec<[u64; 3]> {
        match self.params {
            Parameters::Bitnum => smallvec![u64::from(format.width)],
            Parameters::FixedPoint => {
                let (int_width, frac_width) = format.parts().unwrap();

                smallvec![
                    u64::from(format.width),
                    u64::from(int_width),
                    u64::from(frac_width)
                ]
            }
        }
    }
}

pub mod compile {
    use super::*;

    pub const IMPORT: &str = "primitives/compile.futil";

    pub const STD_ADD: Primitive = Primitive {
        name: "std_add",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };
}

pub mod core {
    use super::*;

    pub const IMPORT: &str = "primitives/core.futil";

    pub const STD_SUB: Primitive = Primitive {
        name: "std_sub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_GT: Primitive = Primitive {
        name: "std_gt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_LT: Primitive = Primitive {
        name: "std_lt",
        prefix_hint: "lt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_EQ: Primitive = Primitive {
        name: "std_eq",
        prefix_hint: "eq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_NEQ: Primitive = Primitive {
        name: "std_neq",
        prefix_hint: "neq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_GE: Primitive = Primitive {
        name: "std_ge",
        prefix_hint: "ge",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_LE: Primitive = Primitive {
        name: "std_le",
        prefix_hint: "le",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };
}

pub mod binary_operators {
    use super::*;

    pub const IMPORT: &str = "primitives/binary_operators.futil";

    pub const STD_FP_ADD: Primitive = Primitive {
        name: "std_fp_add",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
    };

    pub const STD_FP_SUB: Primitive = Primitive {
        name: "std_fp_sub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
    };

    pub const STD_FP_MULT_PIPE: Primitive = Primitive {
        name: "std_fp_mult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: false,
    };

    pub const STD_FP_DIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_fp_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::FixedPoint,
        is_comb: false,
    };

    pub const STD_FP_DIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_fp_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::FixedPoint,
        is_comb: false,
    };

    pub const STD_FP_GT: Primitive = Primitive {
        name: "std_fp_gt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
    };

    pub const STD_FP_SADD: Primitive = Primitive {
        name: "std_fp_sadd",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
    };

    pub const STD_FP_SSUB: Primitive = Primitive {
        name: "std_fp_ssub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
    };

    pub const STD_FP_SMULT_PIPE: Primitive = Primitive {
        name: "std_fp_smult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: false,
    };

    pub const STD_FP_SDIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_fp_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::FixedPoint,
        is_comb: false,
    };

    pub const STD_FP_SDIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_fp_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::FixedPoint,
        is_comb: false,
    };

    pub const STD_FP_SGT: Primitive = Primitive {
        name: "std_fp_sgt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
    };

    pub const STD_FP_SLT: Primitive = Primitive {
        name: "std_fp_slt",
        prefix_hint: "lt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
    };

    pub const STD_MULT_PIPE: Primitive = Primitive {
        name: "std_mult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: false,
    };

    pub const STD_DIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::Bitnum,
        is_comb: false,
    };

    pub const STD_DIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::Bitnum,
        is_comb: false,
    };

    pub const STD_SADD: Primitive = Primitive {
        name: "std_sadd",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_SSUB: Primitive = Primitive {
        name: "std_ssub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_SMULT_PIPE: Primitive = Primitive {
        name: "std_smult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: false,
    };

    pub const STD_SDIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::Bitnum,
        is_comb: false,
    };

    pub const STD_SDIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::Bitnum,
        is_comb: false,
    };

    pub const STD_SGT: Primitive = Primitive {
        name: "std_sgt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_SLT: Primitive = Primitive {
        name: "std_slt",
        prefix_hint: "lt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_SEQ: Primitive = Primitive {
        name: "std_seq",
        prefix_hint: "eq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_SNEQ: Primitive = Primitive {
        name: "std_sneq",
        prefix_hint: "neq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_SGE: Primitive = Primitive {
        name: "std_sge",
        prefix_hint: "ge",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };

    pub const STD_SLE: Primitive = Primitive {
        name: "std_sle",
        prefix_hint: "le",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: true,
    };
}

pub mod math {
    use super::*;

    pub const IMPORT: &str = "primitives/math.futil";

    pub const STD_FP_SQRT: Primitive = Primitive {
        name: "fp_sqrt",
        prefix_hint: "sqrt",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: false,
    };

    pub const STD_SQRT: Primitive = Primitive {
        name: "sqrt",
        prefix_hint: "sqrt",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Bitnum,
        is_comb: false,
    };
}
