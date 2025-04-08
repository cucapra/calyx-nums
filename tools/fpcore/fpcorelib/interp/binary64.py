import math
import operator
import sys
from collections.abc import Callable
from itertools import combinations

from more_itertools import pairwise

from ..ast import Ctx, Lib


def all_windows(f: Callable[[float, float], bool]):
    def variadic(*args: float):
        return all(f(*pair) for pair in pairwise(args))

    return variadic


def all_combinations(f: Callable[[float, float], bool]):
    def variadic(*args: float):
        return all(f(*pair) for pair in combinations(args, 2))

    return variadic


lt = all_windows(operator.lt)
gt = all_windows(operator.gt)
le = all_windows(operator.le)
ge = all_windows(operator.ge)
eq = all_windows(operator.eq)
ne = all_combinations(operator.ne)


LIB_BINARY64: Lib[float] = {
    '+': operator.add,
    '-': lambda a, b=None: -a if b is None else a - b,
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
    'fmax': max,  # handles NaNs incorrectly
    'fmin': min,  # handles NaNs incorrectly
    'copysign': math.copysign,
    'trunc': math.trunc,  # type: ignore
    '<': lt,
    '>': gt,
    '<=': le,
    '>=': ge,
    '==': eq,
    '!=': ne,
    'and': lambda *args: all(args),
    'or': lambda *args: any(args),
    'not': operator.not_,
    'isfinite': math.isfinite,
    'isinf': math.isinf,
    'isnan': math.isnan,
    'cast': float,
}


if sys.version_info >= (3, 11):
    LIB_BINARY64['exp2'] = math.exp2
    LIB_BINARY64['cbrt'] = math.cbrt


ENV_BINARY64: Ctx[float] = {
    'E': math.e,
    'PI': math.pi,
    'PI_2': math.pi / 2,
    'PI_4': math.pi / 4,
    'INFINITY': math.inf,
    'NAN': math.nan,
    'TRUE': True,
    'FALSE': False,
}
