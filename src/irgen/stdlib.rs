//! Standard library primitive declarations.

pub enum Parameters<'a> {
    Bitnum {
        width: &'a str,
    },
    FixedPoint {
        width: &'a str,
        int_width: &'a str,
        frac_width: &'a str,
    },
}

impl<'a> Parameters<'a> {
    const fn bitnum_default() -> Self {
        Self::Bitnum { width: "WIDTH" }
    }

    const fn fixed_point_default() -> Self {
        Self::FixedPoint {
            width: "WIDTH",
            int_width: "INT_WIDTH",
            frac_width: "FRAC_WIDTH",
        }
    }

    pub const fn build_bitnum_params(width: u64) -> [u64; 1] {
        [width]
    }

    pub const fn build_fixed_point_params(
        width: u64,
        int_width: u64,
    ) -> [u64; 3] {
        [width, int_width, width - int_width]
    }
}

pub enum Arguments<'a> {
    Unary { input: &'a str },
    Binary { left: &'a str, right: &'a str },
}

impl<'a> Arguments<'a> {
    const fn unary_default() -> Self {
        Self::Unary { input: "in" }
    }

    const fn binary_default() -> Self {
        Self::Binary {
            left: "left",
            right: "right",
        }
    }
}

pub struct Signature<'a> {
    pub args: Arguments<'a>,
    pub output: &'a str,
}

impl<'a> Signature<'a> {
    const fn unary_default() -> Self {
        Self {
            args: Arguments::unary_default(),
            output: "out",
        }
    }

    const fn binary_default() -> Self {
        Self {
            args: Arguments::binary_default(),
            output: "out",
        }
    }
}

pub struct Primitive<'a> {
    pub name: &'a str,
    pub prefix_hint: &'a str,
    pub signature: Signature<'a>,
    pub params: Parameters<'a>,
    pub is_comb: bool,
}

pub mod compile {
    use super::*;

    pub const IMPORT: &str = "primitives/compile.futil";

    pub const STD_ADD: Primitive = Primitive {
        name: "std_add",
        prefix_hint: "add",
        signature: Signature::binary_default(),
        params: Parameters::bitnum_default(),
        is_comb: true,
    };
}

pub mod core {
    use super::*;

    pub const IMPORT: &str = "primitives/core.futil";

    pub const STD_SUB: Primitive = Primitive {
        name: "std_sub",
        prefix_hint: "sub",
        signature: Signature::binary_default(),
        params: Parameters::bitnum_default(),
        is_comb: true,
    };
}

pub mod binary_operators {
    use super::*;

    pub const IMPORT: &str = "primitives/binary_operators.futil";

    pub const STD_FP_ADD: Primitive = Primitive {
        name: "std_fp_add",
        prefix_hint: "add",
        signature: Signature::binary_default(),
        params: Parameters::fixed_point_default(),
        is_comb: true,
    };

    pub const STD_FP_SUB: Primitive = Primitive {
        name: "std_fp_sub",
        prefix_hint: "sub",
        signature: Signature::binary_default(),
        params: Parameters::fixed_point_default(),
        is_comb: true,
    };

    pub const STD_FP_MULT_PIPE: Primitive = Primitive {
        name: "std_fp_mult_pipe",
        prefix_hint: "mul",
        signature: Signature::binary_default(),
        params: Parameters::fixed_point_default(),
        is_comb: false,
    };

    pub const STD_FP_DIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_fp_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::binary_default(),
            output: "out_remainder",
        },
        params: Parameters::fixed_point_default(),
        is_comb: false,
    };

    pub const STD_FP_DIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_fp_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::binary_default(),
            output: "out_quotient",
        },
        params: Parameters::fixed_point_default(),
        is_comb: false,
    };

    pub const STD_FP_SADD: Primitive = Primitive {
        name: "std_fp_sadd",
        prefix_hint: "add",
        signature: Signature::binary_default(),
        params: Parameters::fixed_point_default(),
        is_comb: true,
    };

    pub const STD_FP_SSUB: Primitive = Primitive {
        name: "std_fp_ssub",
        prefix_hint: "sub",
        signature: Signature::binary_default(),
        params: Parameters::fixed_point_default(),
        is_comb: true,
    };

    pub const STD_FP_SMULT_PIPE: Primitive = Primitive {
        name: "std_fp_smult_pipe",
        prefix_hint: "mul",
        signature: Signature::binary_default(),
        params: Parameters::fixed_point_default(),
        is_comb: false,
    };

    pub const STD_FP_SDIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_fp_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::binary_default(),
            output: "out_remainder",
        },
        params: Parameters::fixed_point_default(),
        is_comb: false,
    };

    pub const STD_FP_SDIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_fp_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::binary_default(),
            output: "out_quotient",
        },
        params: Parameters::fixed_point_default(),
        is_comb: false,
    };

    pub const STD_MULT_PIPE: Primitive = Primitive {
        name: "std_mult_pipe",
        prefix_hint: "mul",
        signature: Signature::binary_default(),
        params: Parameters::bitnum_default(),
        is_comb: false,
    };

    pub const STD_DIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::binary_default(),
            output: "out_remainder",
        },
        params: Parameters::bitnum_default(),
        is_comb: false,
    };

    pub const STD_DIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::binary_default(),
            output: "out_quotient",
        },
        params: Parameters::bitnum_default(),
        is_comb: false,
    };

    pub const STD_SMULT_PIPE: Primitive = Primitive {
        name: "std_smult_pipe",
        prefix_hint: "mul",
        signature: Signature::binary_default(),
        params: Parameters::bitnum_default(),
        is_comb: false,
    };

    pub const STD_SDIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::binary_default(),
            output: "out_remainder",
        },
        params: Parameters::bitnum_default(),
        is_comb: false,
    };

    pub const STD_SDIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::binary_default(),
            output: "out_quotient",
        },
        params: Parameters::bitnum_default(),
        is_comb: false,
    };
}

pub mod math {
    use super::*;

    pub const IMPORT: &str = "primitives/math.futil";

    pub const STD_FP_SQRT: Primitive = Primitive {
        name: "fp_sqrt",
        prefix_hint: "sqrt",
        signature: Signature::unary_default(),
        params: Parameters::fixed_point_default(),
        is_comb: false,
    };

    pub const STD_SQRT: Primitive = Primitive {
        name: "sqrt",
        prefix_hint: "sqrt",
        signature: Signature::unary_default(),
        params: Parameters::bitnum_default(),
        is_comb: false,
    };
}
