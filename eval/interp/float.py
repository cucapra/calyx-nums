import math
import operator
from typing import Optional


def minus(a: float, b: Optional[float] = None) -> float:
    if b is None:
        return -a
    else:
        return a - b


BINARY64 = {
    '+': operator.add,
    '-': minus,
    '*': operator.mul,
    '/': operator.truediv,
    'fabs': math.fabs,
    'exp': math.exp,
    'expm1': math.expm1,
    'log': math.log,
    'log10': math.log10,
    'log2': math.log2,
    'log1p': math.log1p,
    'pow': math.pow,
    'sqrt': math.sqrt,
    'hypot': math.hypot,
    'sin': math.sin,
    'cos': math.cos,
    'tan': math.tan,
    'asin': math.asin,
    'acos': math.acos,
    'atan': math.atan,
    'atan2': math.atan2,
    'sinh': math.sinh,
    'cosh': math.cosh,
    'tanh': math.tanh,
    'asinh': math.asinh,
    'acosh': math.acosh,
    'atanh': math.atanh,
    'erf': math.erf,
    'erfc': math.erfc,
    'tgamma': math.gamma,
    'lgamma': math.lgamma,
    'ceil': math.ceil,
    'floor': math.floor,
    'fmod': math.fmod,
    'remainder': math.remainder,
    'copysign': math.copysign,
    'cast': float,
}
