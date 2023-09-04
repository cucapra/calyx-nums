from collections.abc import Callable, Iterable
from dataclasses import dataclass
from itertools import chain
from typing import Generic, Optional, SupportsFloat, TypeVar

Num = TypeVar('Num', bound=SupportsFloat)

Ctx = dict[str, Num]
Lib = dict[str, Callable[..., Num]]

class Expr(Generic[Num]):
    def interp(self, context: Ctx[Num], libm: Lib[Num]) -> Num:
        raise NotImplementedError

    def __str__(self) -> str:
        raise NotImplementedError

@dataclass
class Symbol(Expr[Num]):
    id: str

    def interp(self, context: Ctx[Num], libm: Lib[Num]) -> Num:
        return libm['cast'](context[self.id])

    def __str__(self) -> str:
        return self.id

@dataclass
class Number(Expr[Num]):
    val: Num

    def interp(self, context: Ctx[Num], libm: Lib[Num]) -> Num:
        return libm['cast'](self.val)

    def __str__(self) -> str:
        val = float(self.val)

        return f'{val:.0f}' if val.is_integer() else val.hex()

@dataclass
class Operation(Expr[Num]):
    op: str
    args: list[Expr[Num]]

    def interp(self, context: Ctx[Num], libm: Lib[Num]) -> Num:
        args = [arg.interp(context, libm) for arg in self.args]

        return libm[self.op](*args)

    def __str__(self) -> str:
        args = ' '.join(map(str, self.args))

        return f'({self.op} {args})'

@dataclass
class Binder(Generic[Num]):
    var: str
    expr: Expr[Num]

    def __str__(self) -> str:
        return f'[{self.var} {self.expr}]'

@dataclass
class Let(Expr[Num]):
    binders: list[Binder[Num]]
    body: Expr[Num]
    seq: bool

    def interp(self, context: Ctx[Num], libm: Lib[Num]) -> Num:
        scope = context.copy()

        for binder in self.binders:
            inner = scope if self.seq else context

            scope[binder.var] = binder.expr.interp(inner, libm)

        return self.body.interp(scope, libm)

    def __str__(self) -> str:
        binders = ' '.join(map(str, self.binders))
        star = '*' if self.seq else ''

        return f'(let{star} ({binders}) {self.body})'

@dataclass
class Property(Generic[Num]):
    name: str
    data: Expr[Num]

    def __str__(self) -> str:
        return f'{self.name} {self.data}'

@dataclass
class Annotation(Expr[Num]):
    props: list[Property[Num]]
    body: Expr[Num]

    def interp(self, context: Ctx[Num], libm: Lib[Num]) -> Num:
        return self.body.interp(context, libm)

    def __str__(self) -> str:
        props = ' '.join(map(str, self.props))

        return f'(! {props} {self.body})'

@dataclass
class Argument(Generic[Num]):
    var: str
    props: list[Property[Num]]

    def __str__(self) -> str:
        if len(self.props) > 0:
            props = ' '.join(map(str, self.props))

            return f'(! {props} {self.var})'
        else:
            return self.var

@dataclass
class FPCore(Generic[Num]):
    name: Optional[str]
    args: list[Argument[Num]]
    props: list[Property[Num]]
    body: Expr[Num]

    def interp(self, args: Iterable[Num], libm: Lib[Num]) -> Num:
        context = {arg.var: val for arg, val in zip(self.args, args)}

        return self.body.interp(context, libm)

    def __str__(self) -> str:
        name = self.name or ''
        args = ' '.join(map(str, self.args))
        props = ' '.join(chain(map(str, self.props), ('',)))

        return f'(FPCore {name}({args}) {props}{self.body})'
