//! Arithmetic operations on rationals.

use std::cmp::Ordering;
use std::ops::{Add, BitXor, Div, Mul, Neg, Sub};

use num::bigint::BigUint;
use num::integer::Integer;
use num::rational::Ratio;
use num::traits::{Pow, Zero};

use crate::fpcore::ast::{Rational, Sign};

impl Neg for Sign {
    type Output = Self;

    fn neg(self) -> Sign {
        match self {
            Sign::Neg => Sign::Pos,
            Sign::Pos => Sign::Neg,
        }
    }
}

impl BitXor for Sign {
    type Output = Self;

    fn bitxor(self, rhs: Sign) -> Sign {
        if self == rhs {
            Sign::Pos
        } else {
            Sign::Neg
        }
    }
}

impl Add for Rational {
    type Output = Self;

    fn add(self, rhs: Rational) -> Rational {
        if self.sign == rhs.sign {
            return Rational {
                sign: self.sign,
                value: self.value + rhs.value,
            };
        }

        match self.value.cmp(&rhs.value) {
            Ordering::Greater => Rational {
                sign: self.sign,
                value: self.value - rhs.value,
            },
            Ordering::Equal => Rational {
                sign: Sign::Pos,
                value: Zero::zero(),
            },
            Ordering::Less => Rational {
                sign: rhs.sign,
                value: rhs.value - self.value,
            },
        }
    }
}

impl Neg for Rational {
    type Output = Self;

    fn neg(self) -> Rational {
        Rational {
            sign: -self.sign,
            value: self.value,
        }
    }
}

impl Sub for Rational {
    type Output = Self;

    fn sub(self, rhs: Rational) -> Rational {
        self + -rhs
    }
}

impl Mul for Rational {
    type Output = Self;

    fn mul(self, rhs: Rational) -> Rational {
        Rational {
            sign: self.sign ^ rhs.sign,
            value: self.value * rhs.value,
        }
    }
}

impl Div for Rational {
    type Output = Self;

    fn div(self, rhs: Rational) -> Rational {
        Rational {
            sign: self.sign ^ rhs.sign,
            value: self.value / rhs.value,
        }
    }
}

impl<RHS> Pow<RHS> for Rational
where
    RHS: Integer,
    Ratio<BigUint>: Pow<RHS, Output = Ratio<BigUint>>,
{
    type Output = Self;

    fn pow(self, rhs: RHS) -> Rational {
        let sign = if rhs.is_even() { Sign::Pos } else { self.sign };

        Rational {
            sign,
            value: self.value.pow(rhs),
        }
    }
}

impl PartialEq for Rational {
    fn eq(&self, other: &Rational) -> bool {
        if self.sign == other.sign {
            self.value == other.value
        } else {
            self.is_zero() && other.is_zero()
        }
    }
}

impl Eq for Rational {}

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Rational) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rational {
    fn cmp(&self, other: &Rational) -> Ordering {
        if self.is_zero() && other.is_zero() {
            return Ordering::Equal;
        }

        match (self.sign, other.sign) {
            (Sign::Pos, Sign::Pos) => self.value.cmp(&other.value),
            (Sign::Pos, Sign::Neg) => Ordering::Greater,
            (Sign::Neg, Sign::Pos) => Ordering::Less,
            (Sign::Neg, Sign::Neg) => self.value.cmp(&other.value).reverse(),
        }
    }
}

impl<T> From<T> for Rational
where
    BigUint: From<T>,
{
    fn from(value: T) -> Rational {
        Rational {
            sign: Sign::Pos,
            value: Ratio::from_integer(value.into()),
        }
    }
}
