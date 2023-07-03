import re
from dataclasses import dataclass
from fixedpoint import FixedPoint
from typing import Any

MODE = {
    'overflow': 'wrap',
    'rounding': 'down',
    'overflow_alert': 'ignore',
    'mismatch_alert': 'error',
    'implicit_cast_alert': 'error'
}

@dataclass
class QFormat:
    int_width: int
    frac_width: int
    is_signed: bool

    @classmethod
    def parse(cls, format: str):
        if match := re.match(r'^(U)?Q(\d+).(\d+)$', format):
            u, m, n = match.groups()

            return cls(int(m), int(n), u is None)
        else:
            raise RuntimeError('Invalid format')

    @property
    def width(self) -> int:
        return self.int_width + self.frac_width

    def decode(self, bits: int, mode: dict[str, Any] = MODE) -> FixedPoint:
        return FixedPoint(
            hex(bits),
            signed=self.is_signed,
            m=self.int_width,
            n=self.frac_width,
            **mode
        )

    def cast(self, x: FixedPoint) -> FixedPoint:
        x.resize(self.int_width, self.frac_width)

        return x

    def __str__(self) -> str:
        prefix = 'UQ'[self.is_signed:]

        return f'{prefix}{self.int_width}.{self.frac_width}'
