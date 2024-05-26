//! Minimax approximations.

use crate::fpcore::ast::Rational;
use crate::utils::sollya::{self, ScriptError, SollyaFunction};

/// Constructs a table of polynomials approximating `f` piecewise over the
/// interval `[left, right]`.
///
/// The interval is subdivided at evenly-spaced breakpoints, with the number of
/// subintervals given by `size`. Each polynomial is computed so as to minimize
/// the maximum absolute error over the corresponding subinterval.
pub fn build_table(
    f: SollyaFunction,
    degree: u32,
    left: &Rational,
    right: &Rational,
    size: u32,
    scale: i32,
) -> Result<Vec<Vec<Rational>>, ScriptError> {
    let cmd = include_bytes!("scripts/remez.sollya");

    let args = [
        format!("{f}(_x_)"),
        format!("{degree}"),
        format!("{left}"),
        format!("{right}"),
        format!("{size}"),
        format!("{scale}"),
    ];

    let result = sollya::sollya(cmd, &args)?;

    result
        .lines()
        .map(|line| {
            line.split(' ')
                .map(|c| {
                    Rational::from_dyadic(c)
                        .map_err(|_| ScriptError::BadResponse)
                })
                .collect()
        })
        .collect()
}
