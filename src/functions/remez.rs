//! Minimax approximations.

use calyx_utils::{CalyxResult, Error};
use itertools::PeekingNext;
use num::bigint::{BigInt, BigUint, ParseBigIntError};
use num::rational::Ratio;
use num::traits::Pow;

use crate::format::Format;
use crate::fpcore::ast::{Rational, Sign};
use crate::utils::sollya::{self, SollyaFunction};

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
    format: &Format,
) -> CalyxResult<Vec<Vec<Rational>>> {
    let cmd = include_bytes!("scripts/remez.sollya");

    let args = [
        format!("{f}(_x_)"),
        format!("{degree}"),
        format!("{left}"),
        format!("{right}"),
        format!("{size}"),
        format!("{}", format.frac_width),
    ];

    let result = sollya::sollya(cmd, &args)
        .map_err(|err| Error::misc(format!("Sollya error: {err}")))?;

    result
        .lines()
        .map(|line| {
            line.split(' ')
                .map(|c| {
                    parse_dyadic(c).map_err(|_| {
                        Error::misc(format!(
                            "Sollya error: failed to parse coefficient `{c}`"
                        ))
                    })
                })
                .collect()
        })
        .collect()
}

/// Parses a number in Gappa dyadic notation.
///
/// The accepted format is defined by the following regular expression:
/// `[-+]?[0-9]+([bB][-+]?[0-9]+)?`.
fn parse_dyadic(s: &str) -> Result<Rational, ParseBigIntError> {
    let mut iter = s.chars();

    let sign = match iter.peeking_next(|&c| c == '-' || c == '+') {
        Some('-') => Sign::Neg,
        _ => Sign::Pos,
    };

    let rest = iter.as_str();

    if let Some((m, e)) = rest.split_once(|c| c == 'b' || c == 'B') {
        let mantissa: BigUint = m.parse()?;
        let exponent: BigInt = e.parse()?;

        let value = Ratio::from(mantissa)
            * Ratio::from(BigUint::from(2u8)).pow(exponent);

        Ok(Rational { sign, value })
    } else {
        Ok(Rational {
            sign,
            value: Ratio::from_integer(rest.parse()?),
        })
    }
}
