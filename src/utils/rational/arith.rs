//! Arithmetic operations on rationals.

use std::cmp::Ordering;
use std::ops::{Add, BitXor, Div, Mul, Neg, Sub};

use num::bigint::BigUint;
use num::integer::Integer;
use num::rational::Ratio;
use num::traits::{One, Pow, Zero};

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

impl Rational {
    /// Creates a `Rational` with value 2^`k`.
    pub fn power_of_two(k: i64) -> Rational {
        let mut val = BigUint::zero();

        val.set_bit(k.unsigned_abs(), true);

        if k >= 0 {
            Rational::new(Sign::Pos, val, One::one())
        } else {
            Rational::new(Sign::Pos, One::one(), val)
        }
    }

    /// Rounds towards zero.
    pub fn truncate(&self, lsb: i64) -> Rational {
        let numer = self.mag.numer();
        let denom = self.mag.denom();

        if lsb < 0 {
            let shift = lsb.unsigned_abs();
            let val = (numer << shift) / denom;

            Rational::new(self.sign, val, BigUint::one() << shift)
        } else {
            let val = numer / (denom << lsb);

            Rational::new(self.sign, val << lsb, BigUint::one())
        }
    }

    /// Rounds away from zero.
    pub fn round_away(&self, lsb: i64) -> Rational {
        let numer = self.mag.numer();
        let denom = self.mag.denom();

        if lsb < 0 {
            let shift = lsb.unsigned_abs();
            let val = (numer << shift).div_ceil(denom);

            Rational::new(self.sign, val, BigUint::one() << shift)
        } else {
            let val = numer.div_ceil(&(denom << lsb));

            Rational::new(self.sign, val << lsb, BigUint::one())
        }
    }

    /// Rounds towards negative infinity.
    pub fn floor(&self, lsb: i64) -> Rational {
        if self.is_negative() {
            self.round_away(lsb)
        } else {
            self.truncate(lsb)
        }
    }

    /// Rounds towards positive infinity.
    pub fn ceil(&self, lsb: i64) -> Rational {
        if self.is_negative() {
            self.truncate(lsb)
        } else {
            self.round_away(lsb)
        }
    }

    /// Computes the floor of the base-2 log of `self`.
    ///
    /// # Panics
    ///
    /// Panics if `self` is not strictly positive.
    pub fn floor_log2(&self) -> i64 {
        assert!(!self.is_negative() && !self.is_zero());

        let numer = self.mag.numer();
        let denom = self.mag.denom();

        let n_bits = numer.bits();
        let d_bits = denom.bits();

        let bump = match n_bits.cmp(&d_bits) {
            Ordering::Greater => numer < &(denom << (n_bits - d_bits)),
            Ordering::Equal => numer < denom,
            Ordering::Less => &(numer << (d_bits - n_bits)) < denom,
        };

        (n_bits - bump as u64) as i64 - d_bits as i64
    }

    /// Computes the ceiling of the base-2 log of `self`.
    ///
    /// # Panics
    ///
    /// Panics if `self` is not strictly positive.
    pub fn ceil_log2(&self) -> i64 {
        assert!(!self.is_negative() && !self.is_zero());

        let numer = self.mag.numer();
        let denom = self.mag.denom();

        let n_bits = numer.bits();
        let d_bits = denom.bits();

        let bump = match n_bits.cmp(&d_bits) {
            Ordering::Greater => numer > &(denom << (n_bits - d_bits)),
            Ordering::Equal => numer > denom,
            Ordering::Less => &(numer << (d_bits - n_bits)) > denom,
        };

        n_bits as i64 - (d_bits - bump as u64) as i64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rational_from_prims(sign: Sign, numer: u8, denom: u8) -> Rational {
        Rational::new(sign, numer.into(), denom.into())
    }

    #[test]
    fn rounding() {
        let pos_five_quarters = rational_from_prims(Sign::Pos, 5, 4);
        let neg_five_quarters = rational_from_prims(Sign::Neg, 5, 4);

        for mode in [Rational::floor, Rational::ceil] {
            for val in [&pos_five_quarters, &neg_five_quarters] {
                assert_eq!(&mode(val, -2), val);
            }
        }

        assert_eq!(
            pos_five_quarters.ceil(-1),
            rational_from_prims(Sign::Pos, 3, 2)
        );
        assert_eq!(
            pos_five_quarters.ceil(1),
            rational_from_prims(Sign::Pos, 2, 1)
        );

        assert_eq!(
            pos_five_quarters.floor(-1),
            rational_from_prims(Sign::Pos, 1, 1)
        );
        assert_eq!(
            pos_five_quarters.floor(1),
            rational_from_prims(Sign::Pos, 0, 1)
        );

        assert_eq!(
            neg_five_quarters.ceil(-1),
            rational_from_prims(Sign::Neg, 1, 1)
        );
        assert_eq!(
            neg_five_quarters.ceil(1),
            rational_from_prims(Sign::Neg, 0, 1)
        );

        assert_eq!(
            neg_five_quarters.floor(-1),
            rational_from_prims(Sign::Neg, 3, 2)
        );
        assert_eq!(
            neg_five_quarters.floor(1),
            rational_from_prims(Sign::Neg, 2, 1)
        );
    }

    #[test]
    fn floor_log2() {
        assert_eq!(rational_from_prims(Sign::Pos, 0b111, 16).floor_log2(), -2);
        assert_eq!(rational_from_prims(Sign::Pos, 1, 2).floor_log2(), -1);

        assert_eq!(rational_from_prims(Sign::Pos, 0b101, 2).floor_log2(), 1);
        assert_eq!(rational_from_prims(Sign::Pos, 0b100, 1).floor_log2(), 2);

        assert_eq!(rational_from_prims(Sign::Pos, 1, 16).floor_log2(), -4);
        assert_eq!(rational_from_prims(Sign::Pos, 1, 8).floor_log2(), -3);

        assert_eq!(rational_from_prims(Sign::Pos, 11, 5).floor_log2(), 1);
        assert_eq!(rational_from_prims(Sign::Pos, 1, 3).floor_log2(), -2);
    }

    #[test]
    fn ceil_log2() {
        assert_eq!(rational_from_prims(Sign::Pos, 0b1011, 16).ceil_log2(), 0);
        assert_eq!(rational_from_prims(Sign::Pos, 1, 2).ceil_log2(), -1);

        assert_eq!(rational_from_prims(Sign::Pos, 0b1001, 2).ceil_log2(), 3);
        assert_eq!(rational_from_prims(Sign::Pos, 0b100, 1).ceil_log2(), 2);

        assert_eq!(rational_from_prims(Sign::Pos, 0b11, 16).ceil_log2(), -2);
        assert_eq!(rational_from_prims(Sign::Pos, 1, 8).ceil_log2(), -3);

        assert_eq!(rational_from_prims(Sign::Pos, 11, 5).ceil_log2(), 2);
        assert_eq!(rational_from_prims(Sign::Pos, 1, 3).ceil_log2(), -1);
    }
}
