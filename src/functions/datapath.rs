//! Polynomial evaluator sizing.

use std::{cmp, iter};

use super::TableDomain;
use crate::fpcore::ast::Rational;
use crate::utils::interval::Interval;
use crate::utils::mangling::Mangle;

/// Architectural parameters for a polynomial evaluator.
#[derive(Mangle)]
pub struct Datapath {
    pub lut_widths: Vec<u32>,
    pub product_width: u32,
    pub sum_width: u32,
}

impl Datapath {
    pub fn from_table(
        table: &[Vec<Rational>],
        degree: usize,
        domain: &TableDomain,
        scale: i32,
    ) -> Datapath {
        let lut_widths = table_ranges(table, degree, scale);

        let HornerRanges {
            product_width,
            sum_width,
        } = HornerRanges::from_table(table, degree, domain, scale);

        Datapath {
            lut_widths,
            product_width,
            sum_width,
        }
    }
}

pub fn table_ranges(
    table: &[Vec<Rational>],
    degree: usize,
    scale: i32,
) -> Vec<u32> {
    let mut widths = vec![0; degree + 1];

    for row in table {
        for (value, max_width) in iter::zip(row, &mut widths) {
            *max_width = cmp::max(*max_width, signed_width(value, scale));
        }
    }

    widths
}

pub struct HornerRanges {
    pub product_width: u32,
    pub sum_width: u32,
}

impl HornerRanges {
    pub fn from_table(
        table: &[Vec<Rational>],
        degree: usize,
        domain: &TableDomain,
        scale: i32,
    ) -> HornerRanges {
        let stride =
            (&domain.right - &domain.left) / Rational::from(table.len());

        let init = HornerRanges {
            product_width: 0,
            sum_width: 0,
        };

        table.iter().fold(init, |max, approx| {
            let HornerRanges {
                product_width,
                sum_width,
            } = HornerRanges::from_approx(approx, degree, &stride, scale);

            let product_width = cmp::max(max.product_width, product_width);
            let sum_width = cmp::max(max.sum_width, sum_width);

            HornerRanges {
                product_width,
                sum_width,
            }
        })
    }

    // See `computeHornerMSBs` (de Dinechin, 2015).
    fn from_approx(
        approx: &[Rational],
        degree: usize,
        stride: &Rational,
        scale: i32,
    ) -> HornerRanges {
        let mut product_width = 0;
        let mut sum_width = signed_width(&approx[degree], scale);

        let mut sigma = Interval::from(&approx[degree]);

        for i in (0..degree).rev() {
            let pi =
                Interval::new(Rational::from(0u32), stride.clone()) * sigma;

            product_width = product_width
                .max(signed_width(&pi.inf, scale))
                .max(signed_width(&pi.sup, scale));

            sigma = Interval::from(&approx[i]) + pi;

            sum_width = sum_width
                .max(signed_width(&sigma.inf, scale))
                .max(signed_width(&sigma.sup, scale));
        }

        HornerRanges {
            product_width,
            sum_width,
        }
    }
}

fn signed_width(x: &Rational, scale: i32) -> u32 {
    if x.is_zero() {
        return 0;
    }

    let msb = if x.is_negative() {
        x.ceil_log2_abs()
    } else {
        x.floor_log2_abs() + 1
    };

    let msb = i32::try_from(msb).unwrap();

    if msb >= scale {
        msb.abs_diff(scale) + 1
    } else {
        0
    }
}
