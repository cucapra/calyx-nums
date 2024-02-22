//! Interval arithmetic.

use std::cmp::{self, Ordering};
use std::ops::{Add, Mul, Neg, Sub};

use num::Zero;

use crate::fpcore::ast::{Rational, Sign};

/// Class of a nonempty interval (Hickey et al., 2001).
enum Class {
    M,
    Z,
    P,
    N,
}

/// A closed interval.
pub struct Interval {
    pub inf: Rational,
    pub sup: Rational,
}

impl Interval {
    pub fn new(inf: Rational, sup: Rational) -> Interval {
        Interval { inf, sup }
    }

    fn classify(&self) -> Class {
        // See Hickey et al. (2001), Sec. 4.3.
        match (self.inf.sgn(), self.sup.sgn()) {
            (Ordering::Less, Ordering::Greater) => Class::M,
            (Ordering::Equal, Ordering::Equal) => Class::Z,
            (_, Ordering::Greater) => Class::P,
            (Ordering::Less, _) => Class::N,
            _ => unreachable!(),
        }
    }
}

impl From<&Rational> for Interval {
    fn from(value: &Rational) -> Interval {
        Interval::new(value.clone(), value.clone())
    }
}

impl Neg for Interval {
    type Output = Self;

    fn neg(self) -> Interval {
        Interval {
            inf: -self.sup,
            sup: -self.inf,
        }
    }
}

impl Add for Interval {
    type Output = Self;

    fn add(self, rhs: Interval) -> Interval {
        Interval {
            inf: self.inf + rhs.inf,
            sup: self.sup + rhs.sup,
        }
    }
}

impl Sub for Interval {
    type Output = Self;

    fn sub(self, rhs: Interval) -> Interval {
        Interval {
            inf: self.inf - rhs.sup,
            sup: self.sup - rhs.inf,
        }
    }
}

impl Mul for Interval {
    type Output = Self;

    fn mul(self, rhs: Interval) -> Interval {
        let lhs_class = self.classify();
        let rhs_class = rhs.classify();

        let Interval { inf: a, sup: b } = self;
        let Interval { inf: c, sup: d } = rhs;

        // See Hickey et al. (2001), Thm. 6.
        match (lhs_class, rhs_class) {
            (Class::P, Class::P) => Interval::new(a * c, b * d),
            (Class::P, Class::M) => Interval::new(&b * c, b * d),
            (Class::P, Class::N) => Interval::new(b * c, a * d),
            (Class::M, Class::P) => Interval::new(a * &d, b * d),
            (Class::M, Class::M) => Interval {
                inf: cmp::min(&a * &d, &b * &c),
                sup: cmp::max(a * c, b * d),
            },
            (Class::M, Class::N) => Interval::new(b * &c, a * c),
            (Class::N, Class::P) => Interval::new(a * d, b * c),
            (Class::N, Class::M) => Interval::new(&a * d, a * c),
            (Class::N, Class::N) => Interval::new(b * d, a * c),
            (Class::Z, _) | (_, Class::Z) => {
                let zero = |sign| Rational {
                    sign,
                    mag: Zero::zero(),
                };

                Interval::new(zero(Sign::Pos), zero(Sign::Neg))
            }
        }
    }
}
