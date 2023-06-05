import math
import re
from dataclasses import dataclass
from fixedpoint import FixedPoint

FXP_MODE = {
    'overflow': 'wrap',
    'rounding': 'down',
    'overflow_alert': 'ignore',
    'mismatch_alert': 'error',
    'implicit_cast_alert': 'error'
}

@dataclass(init=False, repr=False)
class QFormat:
    int_width: int
    frac_width: int
    is_signed: bool

    def __init__(self, format: str) -> None:
        if match := re.match(r'^(U)?Q(\d+).(\d+)$', format):
            u, m, n = match.groups()

            self.int_width = int(m)
            self.frac_width = int(n)
            self.is_signed = u is None
        else:
            raise RuntimeError('Invalid format')

    @property
    def width(self) -> int:
        return self.int_width + self.frac_width

    def decode(self, bits: int) -> FixedPoint:
        return FixedPoint(
            hex(bits),
            signed=self.is_signed,
            m=self.int_width,
            n=self.frac_width,
            **FXP_MODE
        )

    def cast(self, x: FixedPoint) -> FixedPoint:
        x.resize(self.int_width, self.frac_width)

        return x

    def __str__(self) -> str:
        prefix = 'UQ'[self.is_signed:]

        return f'{prefix}{self.int_width}.{self.frac_width}'

def sqrt(x: FixedPoint) -> FixedPoint:
    new = FixedPoint(x)
    new.from_str(hex(math.isqrt(x.bits << x.n)))

    return new
