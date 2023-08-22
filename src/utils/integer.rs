//! Integer functions.

use num::traits::{PrimInt, Unsigned};

/// Computes the ceiling of the base-2 log of `x`. Panics if `x` is zero.
pub fn ceil_log2<T>(x: T) -> u32
where
    T: PrimInt + Unsigned,
{
    assert!(!x.is_zero());

    T::zero().count_zeros() - T::leading_zeros(x - T::one())
}
