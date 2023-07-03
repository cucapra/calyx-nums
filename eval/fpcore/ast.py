import operator
import random
from dataclasses import dataclass
from typing import Callable, Generic, Iterable, Optional, SupportsFloat, TypeVar

import libm.fixed as fixed
from libm.qformat import FixedPoint, QFormat

Fxp = FixedPoint
Num = TypeVar('Num', bound=SupportsFloat)

class Expr(Generic[Num]):
    def interp(self, context: dict[str, Num]) -> Num:
        raise NotImplementedError

    def __str__(self) -> str:
        raise NotImplementedError

@dataclass
class Symbol(Expr[Num]):
    id: str

    def interp(self, context: dict[str, Num]) -> Num:
        return context[self.id]

    def __str__(self) -> str:
        return self.id

@dataclass
class Number(Expr[Num]):
    val: Num

    def interp(self, context: dict[str, Num]) -> Num:
        return self.val

    def __str__(self) -> str:
        return float(self.val).hex()

@dataclass
class Operation(Expr[Num]):
    op: str
    impl: Callable[..., Num]
    args: list[Expr[Num]]

    def interp(self, context: dict[str, Num]) -> Num:
        args = [arg.interp(context) for arg in self.args]

        return self.impl(*args)

    def __str__(self) -> str:
        args = ' '.join(map(str, self.args))

        return f'({self.op} {args})'

@dataclass
class FPCore(Generic[Num]):
    name: Optional[str]
    args: list[str]
    body: Expr[Num]

    def interp(self, args: Iterable[Num]) -> Num:
        return self.body.interp(dict(zip(self.args, args)))

    def __str__(self) -> str:
        name = '' if self.name is None else self.name
        args = ' '.join(self.args)

        return f'(FPCore {name}({args}) {self.body})'

def random_sym(vars: list[str]) -> Symbol[Fxp]:
    return Symbol(random.choice(vars))

def random_num(fmt: QFormat) -> Number[Fxp]:
    return Number(fmt.decode(random.getrandbits(fmt.width)))

def _wrap(fn: Callable[..., Fxp], fmt: QFormat):
    return lambda *args: fmt.cast(fn(*args))

def random_op(vars: list[str], lpi: float, fmt: QFormat) -> Operation[Fxp]:
    op, impl, arity = random.choice((
        ('+', _wrap(operator.add, fmt), 2),
        ('-', _wrap(operator.sub, fmt), 2),
        ('*', _wrap(operator.mul, fmt), 2),
        ('sqrt', fixed.sqrt, 1),
    ))

    args = [random_expr(vars, lpi / arity, fmt) for _ in range(arity)]

    return Operation(op, impl, args)

def random_expr(vars: list[str], lpi: float, fmt: QFormat) -> Expr[Fxp]:
    if random.random() < 1 / lpi:
        if random.getrandbits(1):
            return random_sym(vars)
        else:
            return random_num(fmt)
    else:
        return random_op(vars, lpi, fmt)

def random_fpcore(
    name: Optional[str], args: list[str], lpi: float, fmt: QFormat
) -> FPCore[Fxp]:
    body = random_expr(args, lpi, fmt)

    return FPCore(name, args, body)
