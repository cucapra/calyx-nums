//! Dyadic notation.

use std::fmt;

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

    /// Returns an adapter for formatting `self` in Gappa dyadic notation.
    ///
    /// This method is infallible. However, attempting to format a non-dyadic
    /// rational with the returned adapter will result in a panic.
    pub fn dyadic(&self) -> impl fmt::Display + '_ {
        struct Dyadic<'a>(&'a Rational);

        impl fmt::Display for Dyadic<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let exponent = self.0.frac_width().unwrap();

                if self.0.is_negative() {
                    write!(f, "-")?
                }

                write!(f, "{}", self.0.mag.numer())?;

                if exponent != 0 {
                    write!(f, "b-{}", exponent)
                } else {
                    Ok(())
                }
            }
        }

        Dyadic(self)
    }
}
