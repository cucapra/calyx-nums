//! Arithmetic operations on rationals.

use std::cmp::Ordering;
use std::ops::{Add, BitXor, Div, Mul, Neg, Sub};

use num::{rational::Ratio, BigUint, Zero};

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
