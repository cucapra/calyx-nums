import math
import operator
from fixedpoint import FixedPoint
from typing import Any, Callable

def sqrt(x: FixedPoint) -> FixedPoint:
    new = FixedPoint(x)
    new.from_str(hex(math.isqrt(x.bits << x.n)))

    return new

def resize(x: FixedPoint, m: int, n: int) -> FixedPoint:
    x.resize(m, n)

    return x

def _wrap(fn: Callable[..., FixedPoint]):
    return lambda x, *args: resize(fn(x, *args), x.m, x.n)

FIXED: dict[str, Any] = {
    '+': _wrap(operator.add),
    '-': _wrap(operator.sub),
    '*': _wrap(operator.mul),
    'sqrt': sqrt,
    'cast': lambda x: x,
}
