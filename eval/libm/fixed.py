import math
from fixedpoint import FixedPoint

def sqrt(x: FixedPoint) -> FixedPoint:
    new = FixedPoint(x)
    new.from_str(hex(math.isqrt(x.bits << x.n)))

    return new
