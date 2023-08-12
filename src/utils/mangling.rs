//! Name mangling.

use super::sollya::SollyaFunction;
use crate::format::Format;
use crate::fpcore::ast::Rational;
use crate::fpcore::metadata::{CalyxDomain, CalyxImpl};

/// See `mangle_name`.
pub fn mangle_function(
    f: SollyaFunction,
    format: &Format,
    domain: &CalyxDomain,
    strategy: &CalyxImpl,
) -> String {
    mangle_name(f.as_str(), format, domain, strategy)
}

/// Encodes context information into an identifier. The resulting identifier is
/// a valid name in the IA-64 C++ ABI's name mangling scheme.
pub fn mangle_name(
    name: &str,
    format: &Format,
    domain: &CalyxDomain,
    strategy: &CalyxImpl,
) -> String {
    format!(
        "_Z{}{}IX{}EX{}EX{}EE",
        name.len(),
        name,
        mangle_format(format),
        mangle_domain(domain),
        mangle_strategy(strategy)
    )
}

fn mangle_format(format: &Format) -> String {
    format!(
        "tl6FormatLj{}ELj{}ELb{}EE",
        format.width, format.frac_width, format.is_signed as u8
    )
}

fn mangle_domain(domain: &CalyxDomain) -> String {
    format!(
        "tl11CalyxDomain{}{}E",
        mangle_rational(&domain.left.value),
        mangle_rational(&domain.right.value)
    )
}

fn mangle_strategy(strategy: &CalyxImpl) -> String {
    let kind = match strategy {
        CalyxImpl::Lut { lut_size } => {
            format!("tl3LutLj{}EE", lut_size)
        }
        CalyxImpl::Poly { degree, lut_size } => {
            format!("tl4PolyLj{}ELj{}EE", degree, lut_size)
        }
    };

    format!("tl9CalyxImpl{}E", kind)
}

fn mangle_rational(rational: &Rational) -> String {
    let sign = if rational.is_negative() { "n" } else { "" };

    format!(
        "tl8RationalLi{}{}ELj{}EE",
        sign,
        rational.mag.numer(),
        rational.mag.denom()
    )
}
