//! Numeric literals.

use std::fmt;

use num::bigint::{BigInt, BigUint, ParseBigIntError};
use num::rational::Ratio;
use num::traits::{Num, Pow, Zero};

use crate::utils::mangling::Mangle;

#[derive(Clone, Copy, Debug, Mangle, PartialEq, Eq)]
pub enum Sign {
    Pos,
    Neg,
}

/// An arbitrary-precision rational number with signed zeros.
#[derive(Clone, Debug)]
pub struct Rational {
    pub sign: Sign,
    pub mag: Ratio<BigUint>,
}

impl Rational {
    /// Convenience constructor.
    pub fn new(sign: Sign, numer: BigUint, denom: BigUint) -> Rational {
        Rational {
            sign,
            mag: Ratio::new(numer, denom),
        }
    }

    /// Create a `Rational` from a numerator and denominator.
    pub fn from_ratio_str(
        sign: Sign,
        numer: &str,
        denom: &str,
        radix: u32,
    ) -> Result<Rational, ParseBigIntError> {
        let numer = BigUint::from_str_radix(numer, radix)?;
        let denom = BigUint::from_str_radix(denom, radix)?;

        Ok(Rational::new(sign, numer, denom))
    }

    /// Create a `Rational` from integer and fractional components.
    pub fn from_fixed_point_str(
        sign: Sign,
        integer: &str,
        fraction: &str,
        radix: u32,
    ) -> Result<Rational, ParseBigIntError> {
        Ok(Rational {
            sign,
            mag: ratio_from_fixed_point_str(integer, fraction, radix)?,
        })
    }

    /// Create a `Rational` from its components in scientific form. The exponent
    /// may be signed and is assumed to be in radix 10 regardless of the radix
    /// of the mantissa.
    pub fn from_scientific_str(
        sign: Sign,
        integer: &str,
        fraction: &str,
        radix: u32,
        base: u32,
        exponent: &str,
    ) -> Result<Rational, ParseBigIntError> {
        Ok(Rational {
            sign,
            mag: ratio_from_scientific_str(
                integer, fraction, radix, base, exponent,
            )?,
        })
    }

    /// Create a `Rational` from a triple of decimal integers `(m, e, b)`
    /// representing the value `m * b^e`. The exponent may be signed.
    pub fn from_digits(
        sign: Sign,
        mantissa: &str,
        exponent: &str,
        base: &str,
    ) -> Result<Rational, ParseBigIntError> {
        let mantissa = BigUint::from_str_radix(mantissa, 10)?;
        let exponent = BigInt::from_str_radix(exponent, 10)?;
        let base = BigUint::from_str_radix(base, 10)?;

        Ok(Rational {
            sign,
            mag: Ratio::from(mantissa) * Ratio::from(base).pow(exponent),
        })
    }

    /// Returns `true` if the number is negative.
    pub fn is_negative(&self) -> bool {
        match self.sign {
            Sign::Pos => false,
            Sign::Neg => true,
        }
    }

    /// Returns `true` if the number is zero.
    pub fn is_zero(&self) -> bool {
        self.mag.is_zero()
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_negative() {
            write!(f, "-")?
        }
        write!(f, "{}", self.mag)
    }
}

fn ratio_from_fixed_point_str(
    integer: &str,
    fraction: &str,
    radix: u32,
) -> Result<Ratio<BigUint>, ParseBigIntError> {
    let fraction = fraction.trim_end_matches('0');
    let w = fraction.len();

    let integer = BigUint::from_str_radix(integer, radix)?;

    let fraction = if w != 0 {
        BigUint::from_str_radix(fraction, radix)?
    } else {
        BigUint::zero()
    };

    let denom = BigUint::from(radix).pow(w);
    let numer = integer * &denom + fraction;

    Ok(Ratio::new(numer, denom))
}

fn ratio_from_scientific_str(
    integer: &str,
    fraction: &str,
    radix: u32,
    base: u32,
    exponent: &str,
) -> Result<Ratio<BigUint>, ParseBigIntError> {
    let mantissa = ratio_from_fixed_point_str(integer, fraction, radix)?;
    let exponent = BigInt::from_str_radix(exponent, 10)?;

    Ok(mantissa * Ratio::from_integer(base.into()).pow(exponent))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn fixed_point_constructor() {
        assert_eq!(
            ratio_from_fixed_point_str("1", "42", 10).unwrap(),
            Ratio::from_str("71/50").unwrap()
        );
        assert_eq!(
            ratio_from_fixed_point_str("10", "8", 16).unwrap(),
            Ratio::from_str("33/2").unwrap()
        );
        assert_eq!(
            ratio_from_fixed_point_str("0", "0101010", 2).unwrap(),
            Ratio::from_str("21/64").unwrap()
        );
        assert_eq!(
            ratio_from_fixed_point_str("0Ab", "0", 16).unwrap(),
            Ratio::from_str("171").unwrap()
        );
        assert_eq!(
            ratio_from_fixed_point_str("0", "0", 2).unwrap(),
            Ratio::from_str("0").unwrap()
        );
    }

    #[test]
    fn scientific_constructor() {
        assert_eq!(
            ratio_from_scientific_str("1", "5", 10, 10, "3").unwrap(),
            Ratio::from_str("1500").unwrap()
        );
        assert_eq!(
            ratio_from_scientific_str("1", "8", 16, 2, "-10").unwrap(),
            Ratio::from_str("3/2048").unwrap()
        );
        assert_eq!(
            ratio_from_scientific_str("1", "1", 2, 2, "0").unwrap(),
            Ratio::from_str("3/2").unwrap()
        );
    }
}
