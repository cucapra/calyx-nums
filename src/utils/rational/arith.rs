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

impl Neg for Rational {
    type Output = Self;

    fn neg(self) -> Rational {
        Rational {
            sign: -self.sign,
            mag: self.mag,
        }
    }
}

macro_rules! forwarding_unop {
    ($trait:ident, $method:ident, $type:ty) => {
        impl $trait for &$type {
            type Output = $type;

            fn $method(self) -> $type {
                self.clone().$method()
            }
        }
    };
}

forwarding_unop!(Neg, neg, Rational);

impl Add for Rational {
    type Output = Self;

    fn add(self, rhs: Rational) -> Rational {
        if self.sign == rhs.sign {
            return Rational {
                sign: self.sign,
                mag: self.mag + rhs.mag,
            };
        }

        match self.mag.cmp(&rhs.mag) {
            Ordering::Greater => Rational {
                sign: self.sign,
                mag: self.mag - rhs.mag,
            },
            Ordering::Equal => Rational {
                sign: Sign::Pos,
                mag: Zero::zero(),
            },
            Ordering::Less => Rational {
                sign: rhs.sign,
                mag: rhs.mag - self.mag,
            },
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
            mag: self.mag * rhs.mag,
        }
    }
}

impl Div for Rational {
    type Output = Self;

    fn div(self, rhs: Rational) -> Rational {
        Rational {
            sign: self.sign ^ rhs.sign,
            mag: self.mag / rhs.mag,
        }
    }
}

macro_rules! forwarding_binop {
    ($trait:ident, $method:ident, $type:ty) => {
        impl $trait<&$type> for $type {
            type Output = $type;

            fn $method(self, rhs: &$type) -> $type {
                self.$method(rhs.clone())
            }
        }

        impl $trait<$type> for &$type {
            type Output = $type;

            fn $method(self, rhs: $type) -> $type {
                self.clone().$method(rhs)
            }
        }

        impl $trait<&$type> for &$type {
            type Output = $type;

            fn $method(self, rhs: &$type) -> $type {
                self.clone().$method(rhs.clone())
            }
        }
    };
}

forwarding_binop!(Add, add, Rational);
forwarding_binop!(Sub, sub, Rational);
forwarding_binop!(Mul, mul, Rational);
forwarding_binop!(Div, div, Rational);

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
            mag: self.mag.pow(rhs),
        }
    }
}

impl<RHS> Pow<RHS> for &Rational
where
    Rational: Pow<RHS, Output = Rational>,
{
    type Output = Rational;

    fn pow(self, rhs: RHS) -> Rational {
        self.clone().pow(rhs)
    }
}

impl PartialEq for Rational {
    fn eq(&self, other: &Rational) -> bool {
        if self.sign == other.sign {
            self.mag == other.mag
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
            (Sign::Pos, Sign::Pos) => self.mag.cmp(&other.mag),
            (Sign::Pos, Sign::Neg) => Ordering::Greater,
            (Sign::Neg, Sign::Pos) => Ordering::Less,
            (Sign::Neg, Sign::Neg) => self.mag.cmp(&other.mag).reverse(),
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
            mag: Ratio::from_integer(value.into()),
        }
    }
}
