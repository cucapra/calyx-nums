import math
import operator
import random
from dataclasses import dataclass
from typing import Callable, Iterable

class Expr:
    def interp(self, context: dict[str, int]) -> int:
        raise NotImplementedError

    def __str__(self) -> str:
        raise NotImplementedError

@dataclass
class Symbol(Expr):
    id: str

    def interp(self, context: dict[str, int]) -> int:
        return context[self.id]

    def __str__(self) -> str:
        return self.id

@dataclass
class Number(Expr):
    val: int

    def interp(self, context: dict[str, int]) -> int:
        return self.val

    def __str__(self) -> str:
        return str(self.val)

@dataclass
class Operation(Expr):
    op: str
    impl: Callable[..., int]
    args: list[Expr]

    def interp(self, context: dict[str, int]) -> int:
        args = [arg.interp(context) for arg in self.args]

        return self.impl(*args)

    def __str__(self) -> str:
        args = ' '.join(map(str, self.args))

        return f'({self.op} {args})'

@dataclass
class FPCore:
    args: list[str]
    body: Expr

    def interp(self, args: Iterable[int]) -> int:
        return self.body.interp(dict(zip(self.args, args)))

    def __str__(self) -> str:
        args = ' '.join(self.args)

        return f'(FPCore ({args}) {self.body})'

def _wrap(fn: Callable[..., int], width: int):
    return lambda *args: fn(*args) & ((1 << width) - 1)

def random_op(vars: list[str], lpi: float, lmax: int, width: int) -> Operation:
    op, impl, arity = random.choice((
        ('+', _wrap(operator.add, width), 2),
        ('-', _wrap(operator.sub, width), 2),
        ('*', _wrap(operator.mul, width), 2),
        ('sqrt', _wrap(math.isqrt, width), 1),
    ))

    args = [random_expr(vars, lpi / arity, lmax, width) for _ in range(arity)]

    return Operation(op, impl, args)

def random_expr(vars: list[str], lpi: float, lmax: int, width: int) -> Expr:
    if random.random() < 1 / lpi:
        if random.getrandbits(1):
            return Symbol(random.choice(vars))
        else:
            return Number(random.randint(0, lmax))
    else:
        return random_op(vars, lpi, lmax, width)

def random_fpcore(args: list[str], lpi: float, lmax: int, width: int) -> FPCore:
    body = random_expr(args, lpi, lmax, width)

    return FPCore(args, body)
