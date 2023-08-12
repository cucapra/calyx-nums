//! Dyadic notation.

use itertools::PeekingNext;
use num::bigint::{BigInt, BigUint, ParseBigIntError};
use num::rational::Ratio;
use num::traits::Pow;

use crate::fpcore::ast::{Rational, Sign};

impl Rational {
    /// Parses a number in Gappa dyadic notation.
    ///
    /// The accepted format is defined by the following regular expression:
    /// `[-+]?[0-9]+([bB][-+]?[0-9]+)?`.
    pub fn from_dyadic(s: &str) -> Result<Rational, ParseBigIntError> {
        let mut iter = s.chars();

        let sign = match iter.peeking_next(|&c| c == '-' || c == '+') {
            Some('-') => Sign::Neg,
            _ => Sign::Pos,
        };

        let rest = iter.as_str();

        if let Some((m, e)) = rest.split_once(|c| c == 'b' || c == 'B') {
            let mantissa: BigUint = m.parse()?;
            let exponent: BigInt = e.parse()?;

            let mag = Ratio::from(mantissa)
                * Ratio::from(BigUint::from(2u8)).pow(exponent);

            Ok(Rational { sign, mag })
        } else {
            Ok(Rational {
                sign,
                mag: Ratio::from_integer(rest.parse()?),
            })
        }
    }
}
