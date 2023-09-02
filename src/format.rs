//! Numeric formats.

use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use itertools::PeekingNext;

/// A fixed-point number format.
pub struct Format {
    pub width: u64,
    pub frac_width: u64,
    pub is_signed: bool,
}

impl Default for Format {
    fn default() -> Self {
        Format {
            width: 32,
            frac_width: 0,
            is_signed: false,
        }
    }
}

impl FromStr for Format {
    type Err = ParseFormatError;

    /// Parses a fixed-point format in ARM-style Q notation.
    #[allow(clippy::from_str_radix_10)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.chars();
        let is_signed = iter.peeking_next(|&x| x == 'U').is_none();

        if !matches!(iter.next(), Some('Q')) {
            return Err(ParseFormatError);
        }

        let rest = iter.as_str();
        let split = rest.find('.').ok_or(ParseFormatError)?;

        let int_width = u64::from_str_radix(&rest[..split], 10)?;
        let frac_width = u64::from_str_radix(&rest[split + 1..], 10)?;

        Ok(Format {
            width: int_width + frac_width,
            frac_width,
            is_signed,
        })
    }
}

pub struct ParseFormatError;

impl From<ParseIntError> for ParseFormatError {
    fn from(_: ParseIntError) -> Self {
        ParseFormatError
    }
}

impl fmt::Display for ParseFormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid format")
    }
}
