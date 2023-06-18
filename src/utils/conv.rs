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
            self.to_fixed_point(format.width, format.frac_width)
        } else {
            self.to_unsigned_fixed_point(format.width, format.frac_width)
        }
    }

    /// Computes the unsigned fixed-point representation of `self`.
    pub fn to_unsigned_fixed_point<T: TryFrom<BigUint>>(
        &self,
        width: u64,
        frac_width: u64,
    ) -> Option<T> {
        if self.is_negative() {
            return None;
        }

        let padding = frac_width.checked_sub(self.frac_width()?)?;
        let val = self.value.numer() << padding;

        if val.bits() <= width {
            val.try_into().ok()
        } else {
            None
        }
    }

    /// Computes the signed fixed-point representation of `self`.
    pub fn to_fixed_point<T>(&self, width: u64, frac_width: u64) -> Option<T>
    where
        T: TryFrom<BigUint> + Zero,
    {
        if self.value.is_zero() {
            return Some(Zero::zero());
        }

        let padding = frac_width.checked_sub(self.frac_width()?)?;
        let val = self.value.numer() << padding;

        if self.is_negative() {
            let complement = (BigUint::one() << width).checked_sub(&val)?;

            // Check that val <= 2^(width-1)
            if val <= complement {
                return complement.try_into().ok();
            }
        } else if val.bits() < width {
            return val.try_into().ok();
        }

        None
    }

    fn frac_width(&self) -> Option<u64> {
        let denom = self.value.denom();

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
            rational_from_dec(Sign::NonNeg, "42", "125")
                .to_unsigned_fixed_point::<u32>(32, 16)
                .unwrap(),
            2760704
        );
        assert_eq!(
            rational_from_dec(Sign::NonNeg, "1", "14404296875")
                .to_unsigned_fixed_point::<u32>(32, 31)
                .unwrap(),
            2456813568
        );
        assert_eq!(
            rational_from_dec(Sign::NonNeg, "0", "0")
                .to_unsigned_fixed_point::<u32>(32, 16)
                .unwrap(),
            0
        );
    }

    #[test]
    fn unsigned_fixed_point_conversion_widths() {
        assert_eq!(
            rational_from_dec(Sign::NonNeg, "42", "125")
                .to_unsigned_fixed_point::<u64>(32, 2),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::NonNeg, "1", "1")
                .to_unsigned_fixed_point::<u64>(64, 32),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::NonNeg, "65536", "0")
                .to_unsigned_fixed_point::<u64>(32, 16),
            None
        );
        assert_eq!(
            Rational::new(Sign::NonNeg, One::one(), 0x100000000u64.into())
                .to_unsigned_fixed_point::<u32>(32, 32)
                .unwrap(),
            1
        );
        assert_eq!(
            Rational::new(Sign::NonNeg, 0xffffffffu32.into(), One::one())
                .to_unsigned_fixed_point::<u32>(32, 0)
                .unwrap(),
            0xffffffff
        );
    }

    #[test]
    fn unsigned_fixed_point_conversion_target_type() {
        assert_eq!(
            Rational::new(Sign::NonNeg, 0x100000000u64.into(), One::one())
                .to_unsigned_fixed_point::<u32>(64, 0),
            None
        );
    }

    #[test]
    fn fixed_point_conversion() {
        assert_eq!(
            rational_from_dec(Sign::Neg, "1", "0")
                .to_fixed_point::<u32>(32, 16)
                .unwrap(),
            0xffff0000
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "42", "125")
                .to_fixed_point::<u32>(32, 16)
                .unwrap(),
            4292206592
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "3", "0")
                .to_fixed_point::<u32>(3, 0)
                .unwrap(),
            5
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "4", "0")
                .to_fixed_point::<u32>(3, 0)
                .unwrap(),
            4
        );
    }

    #[test]
    fn fixed_point_conversion_widths() {
        assert_eq!(
            rational_from_dec(Sign::Neg, "5", "0").to_fixed_point::<u32>(3, 0),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "65536", "0")
                .to_fixed_point::<u32>(8, 4),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::NonNeg, "0", "5")
                .to_fixed_point::<u32>(1, 1),
            None
        );
        assert_eq!(
            rational_from_dec(Sign::Neg, "0", "5")
                .to_fixed_point::<u32>(1, 1)
                .unwrap(),
            1
        );
    }
}
