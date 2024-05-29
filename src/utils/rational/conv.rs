//! Numeric conversions.

use num::{BigUint, CheckedSub, One, Zero};

use crate::format::Format;
use crate::fpcore::ast::Rational;

impl Rational {
    /// Computes the fixed-point representation of `self` in the given format.
    pub fn to_format<T>(&self, format: &Format) -> Option<T>
    where
        T: TryFrom<BigUint> + Zero,
    {
        if format.is_signed {
            self.to_fixed_point(format.scale, format.width)
        } else {
            self.to_unsigned_fixed_point(format.scale, format.width)
        }
    }

    /// Computes the unsigned fixed-point representation of `self`.
    fn to_unsigned_fixed_point<T>(&self, scale: i32, width: u32) -> Option<T>
    where
        T: TryFrom<BigUint>,
    {
        if self.is_negative() {
            return None;
        }

        let val = self.abs_scaled_integer(scale)?;

        if val.bits() <= u64::from(width) {
            val.try_into().ok()
        } else {
            None
        }
    }

    /// Computes the signed fixed-point representation of `self`.
    fn to_fixed_point<T>(&self, scale: i32, width: u32) -> Option<T>
    where
        T: TryFrom<BigUint> + Zero,
    {
        if self.is_zero() {
            return Some(Zero::zero());
        }

        let val = self.abs_scaled_integer(scale)?;

        if self.is_negative() {
            let complement = (BigUint::one() << width).checked_sub(&val)?;

            // Check that val <= 2^(width-1)
            if val <= complement {
                return complement.try_into().ok();
            }
        } else if val.bits() < u64::from(width) {
            return val.try_into().ok();
        }

        None
    }

    fn abs_scaled_integer(&self, scale: i32) -> Option<BigUint> {
        let numer = self.mag.numer();
        let denom = self.mag.denom();

        let delta = u64::from(scale.unsigned_abs());

        if scale <= 0 {
            Some(numer << delta.checked_sub(self.frac_width()?)?)
        } else {
            if let Some(zeros) = numer.trailing_zeros() {
                if !denom.is_one() || zeros < delta {
                    return None;
                }
            }

            Some(numer >> delta)
        }
    }

    pub fn frac_width(&self) -> Option<u64> {
        let denom = self.mag.denom();

        if denom.count_ones() == 1 {
            Some(denom.bits() - 1)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fpcore::ast::Sign;

    fn rational_from_dec(sign: Sign, int: &str, frac: &str) -> Rational {
        Rational::from_fixed_point_str(sign, int, frac, 10).unwrap()
    }

    #[test]
    fn unsigned_fixed_point_conversion() {
        assert_eq!(
            rational_from_dec(Sign::Pos, "42", "125")
                .to_unsigned_fixed_point::<u32>(-16, 32)
                .unwrap(),
            2760704
        );
        assert_eq!(
            rational_from_dec(Sign::Pos, "1", "14404296875")
                .to_unsigned_fixed_point::<u32>(-31, 32)
                .unwrap(),
            2456813568
        );
        assert_eq!(
            rational_from_dec(Sign::Pos, "0", "0")
                .to_unsigned_fixed_point::<u32>(-16, 32)
                .unwrap(),
            0
        );
        assert_eq!(
            rational_from_dec(Sign::Pos, "20", "0")
                .to_unsigned_fixed_point::<u32>(2, 32)
                .unwrap(),
            5
        );
    }

    #[test]
    fn unsigned_fixed_point_conversion_widths() {
        assert_eq!(
            rational_from_dec(Sign::Pos, "42", "125")
                .to_unsigned_fixed_point::<u64>(-2, 32),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::Pos, "1", "1")
                .to_unsigned_fixed_point::<u64>(-32, 64),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::Pos, "65536", "0")
                .to_unsigned_fixed_point::<u64>(-16, 32),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::Pos, "3", "0")
                .to_unsigned_fixed_point::<u64>(1, 32),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::Pos, "0", "4")
                .to_unsigned_fixed_point::<u64>(1, 32),
            None
        );
        assert_eq!(
            Rational::new(Sign::Pos, One::one(), 0x100000000u64.into())
                .to_unsigned_fixed_point::<u32>(-32, 32)
                .unwrap(),
            1
        );
        assert_eq!(
            Rational::new(Sign::Pos, 0xffffffffu32.into(), One::one())
                .to_unsigned_fixed_point::<u32>(0, 32)
                .unwrap(),
            0xffffffff
        );
        assert_eq!(
            Rational::new(Sign::Pos, 0x100000000u64.into(), One::one())
                .to_unsigned_fixed_point::<u32>(1, 64)
                .unwrap(),
            0x80000000
        );
    }

    #[test]
    fn unsigned_fixed_point_conversion_target_type() {
        assert_eq!(
            Rational::new(Sign::Pos, 0x100000000u64.into(), One::one())
                .to_unsigned_fixed_point::<u32>(0, 64),
            None
        );
    }

    #[test]
    fn fixed_point_conversion() {
        assert_eq!(
            rational_from_dec(Sign::Neg, "1", "0")
                .to_fixed_point::<u32>(-16, 32)
                .unwrap(),
            0xffff0000
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "42", "125")
                .to_fixed_point::<u32>(-16, 32)
                .unwrap(),
            4292206592
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "3", "0")
                .to_fixed_point::<u32>(0, 3)
                .unwrap(),
            5
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "4", "0")
                .to_fixed_point::<u32>(0, 3)
                .unwrap(),
            4
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "20", "0")
                .to_fixed_point::<u32>(2, 32)
                .unwrap(),
            0xfffffffb
        );
    }

    #[test]
    fn fixed_point_conversion_widths() {
        assert_eq!(
            rational_from_dec(Sign::Neg, "5", "0").to_fixed_point::<u32>(0, 3),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "65536", "0")
                .to_fixed_point::<u32>(-4, 8),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::Pos, "0", "5").to_fixed_point::<u32>(-1, 1),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "0", "5")
                .to_fixed_point::<u32>(-1, 1)
                .unwrap(),
            1
        );
    }
}
