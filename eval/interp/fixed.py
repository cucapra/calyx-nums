import math
from typing import Any, Optional

from fixedpoint import FixedPoint


def add(a: FixedPoint, b: FixedPoint) -> FixedPoint:
    result = a + b
    result.resize(a.m, a.n)

    return result


def mul(a: FixedPoint, b: FixedPoint) -> FixedPoint:
    result = a * b
    result.resize(a.m, a.n)

    return result


def minus(a: FixedPoint, b: Optional[FixedPoint] = None) -> FixedPoint:
    if b is None:
        result = -a
    else:
        result = a - b

    result.resize(a.m, a.n)

    return result


def sqrt(x: FixedPoint) -> FixedPoint:
    new = FixedPoint(x)
    new.from_str(hex(math.isqrt(x.bits << x.n)))

    return new


FIXED: dict[str, Any] = {
    '+': add,
    '-': minus,
    '*': mul,
    'sqrt': sqrt,
    'cast': lambda x: x,
}
