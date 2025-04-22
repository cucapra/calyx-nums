//! Standard library primitive declarations.

use std::ops::Index;
use std::{array, iter};

use smallvec::{SmallVec, smallvec};
use strum::{EnumCount, VariantArray, VariantNames};
use strum_macros::{EnumCount, VariantArray, VariantNames};

use crate::utils::Format;

#[derive(Clone, Copy)]
pub enum Parameters {
    Boolean,
    Integer,
    FixedPoint,
}

struct Arguments;

impl Arguments {
    const UNARY_DEFAULT: &[&str] = &["in"];

    const BINARY_DEFAULT: &[&str] = &["left", "right"];
}

pub struct Signature<'a> {
    pub args: &'a [&'a str],
    pub output: &'a str,
}

impl Signature<'_> {
    const UNARY_DEFAULT: Self = Self {
        args: Arguments::UNARY_DEFAULT,
        output: "out",
    };

    const BINARY_DEFAULT: Self = Self {
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
    pub import: Import,
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
            Parameters::Boolean => smallvec![1],
            Parameters::Integer => smallvec![u64::from(format.width)],
            Parameters::FixedPoint => {
                let (int_width, frac_width) = format.parts().unwrap();

                smallvec![
                    u64::from(format.width),
                    u64::from(int_width),
                    u64::from(frac_width),
                ]
            }
        }
    }
}

pub mod compile {
    use super::*;

    pub const IMPORT: Import = Import::Compile;

    pub const STD_ADD: Primitive = Primitive {
        name: "std_add",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };
}

pub mod core {
    use super::*;

    pub const IMPORT: Import = Import::Core;

    pub const STD_NOT: Primitive = Primitive {
        name: "std_not",
        prefix_hint: "com",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Boolean,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_AND: Primitive = Primitive {
        name: "std_and",
        prefix_hint: "con",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Boolean,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_OR: Primitive = Primitive {
        name: "std_or",
        prefix_hint: "dis",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Boolean,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SUB: Primitive = Primitive {
        name: "std_sub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_GT: Primitive = Primitive {
        name: "std_gt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_LT: Primitive = Primitive {
        name: "std_lt",
        prefix_hint: "lt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_EQ: Primitive = Primitive {
        name: "std_eq",
        prefix_hint: "eq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_NEQ: Primitive = Primitive {
        name: "std_neq",
        prefix_hint: "neq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_GE: Primitive = Primitive {
        name: "std_ge",
        prefix_hint: "ge",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_LE: Primitive = Primitive {
        name: "std_le",
        prefix_hint: "le",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };
}

pub mod binary_operators {
    use super::*;

    pub const IMPORT: Import = Import::BinaryOperators;

    pub const STD_FP_ADD: Primitive = Primitive {
        name: "std_fp_add",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SUB: Primitive = Primitive {
        name: "std_fp_sub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_MULT_PIPE: Primitive = Primitive {
        name: "std_fp_mult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
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
        import: IMPORT,
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
        import: IMPORT,
    };

    pub const STD_FP_GT: Primitive = Primitive {
        name: "std_fp_gt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SADD: Primitive = Primitive {
        name: "std_fp_sadd",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SSUB: Primitive = Primitive {
        name: "std_fp_ssub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SMULT_PIPE: Primitive = Primitive {
        name: "std_fp_smult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
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
        import: IMPORT,
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
        import: IMPORT,
    };

    pub const STD_FP_SGT: Primitive = Primitive {
        name: "std_fp_sgt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SLT: Primitive = Primitive {
        name: "std_fp_slt",
        prefix_hint: "lt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_MULT_PIPE: Primitive = Primitive {
        name: "std_mult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_DIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_DIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SADD: Primitive = Primitive {
        name: "std_sadd",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SSUB: Primitive = Primitive {
        name: "std_ssub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SMULT_PIPE: Primitive = Primitive {
        name: "std_smult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SDIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SDIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SGT: Primitive = Primitive {
        name: "std_sgt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SLT: Primitive = Primitive {
        name: "std_slt",
        prefix_hint: "lt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SEQ: Primitive = Primitive {
        name: "std_seq",
        prefix_hint: "eq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SNEQ: Primitive = Primitive {
        name: "std_sneq",
        prefix_hint: "neq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SGE: Primitive = Primitive {
        name: "std_sge",
        prefix_hint: "ge",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SLE: Primitive = Primitive {
        name: "std_sle",
        prefix_hint: "le",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };
}

pub mod math {
    use super::*;

    pub const IMPORT: Import = Import::Math;

    pub const STD_FP_SQRT: Primitive = Primitive {
        name: "fp_sqrt",
        prefix_hint: "sqrt",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SQRT: Primitive = Primitive {
        name: "sqrt",
        prefix_hint: "sqrt",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };
}

pub mod numbers {
    use super::*;

    pub const IMPORT: Import = Import::Numbers;

    pub const NUM_NEG: Primitive = Primitive {
        name: "num_neg",
        prefix_hint: "neg",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };
}

type ImportRepr = u8;

#[derive(Clone, Copy, EnumCount, VariantArray, VariantNames)]
#[repr(u8)]
pub enum Import {
    #[strum(to_string = "primitives/compile.futil")]
    Compile = 1 << 0,
    #[strum(to_string = "primitives/core.futil")]
    Core = 1 << 1,
    #[strum(to_string = "primitives/binary_operators.futil")]
    BinaryOperators = 1 << 2,
    #[strum(to_string = "primitives/math.futil")]
    Math = 1 << 3,
    #[strum(to_string = "primitives/numbers.futil")]
    Numbers = 1 << 4,
}

impl Import {
    pub const ALL: &[Import] = <Self as VariantArray>::VARIANTS;
    pub const PATHS: &[&str] = <Self as VariantNames>::VARIANTS;

    #[inline]
    const fn mask(self) -> ImportRepr {
        self as ImportRepr
    }

    #[inline]
    const fn index(self) -> usize {
        (self as ImportRepr).trailing_zeros() as usize
    }
}

#[derive(Clone, Copy)]
pub struct ImportSet(ImportRepr);

impl ImportSet {
    #[inline]
    pub const fn new() -> ImportSet {
        ImportSet(0)
    }

    #[inline]
    pub const fn contains(&self, file: Import) -> bool {
        self.0 & file.mask() != 0
    }

    #[inline]
    pub const fn insert(&mut self, file: Import) {
        self.0 |= file.mask();
    }

    pub fn paths(&self) -> impl Iterator<Item = &'static str> {
        iter::zip(Import::ALL, Import::PATHS).filter_map(|(&import, &path)| {
            self.contains(import).then_some(path)
        })
    }

    pub fn paths_from<'a>(
        &self,
        paths: &'a ImportPaths,
    ) -> impl Iterator<Item = &'a str> {
        iter::zip(Import::ALL, &paths.paths).filter_map(|(&import, path)| {
            self.contains(import).then_some(path.as_str())
        })
    }
}

impl Default for ImportSet {
    fn default() -> Self {
        ImportSet::new()
    }
}

pub struct ImportPaths {
    paths: [String; Import::COUNT],
}

impl ImportPaths {
    pub fn new<F>(f: F) -> ImportPaths
    where
        F: FnMut(usize) -> String,
    {
        ImportPaths {
            paths: array::from_fn(f),
        }
    }
}

impl Index<Import> for ImportPaths {
    type Output = String;

    fn index(&self, file: Import) -> &Self::Output {
        &self.paths[file.index()]
    }
}
