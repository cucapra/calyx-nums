import re
from collections.abc import Callable, Iterator
from more_itertools import peekable
from typing import Generic, Optional, TypeVar

from .ast import (
    Annotation,
    Expr,
    FPCore,
    Num,
    Number,
    Operation,
    Property,
    Symbol
)

T = TypeVar('T')

class Parser(Generic[Num]):
    def __init__(self, core: str, ntype: Callable[[str], Num]):
        self.tokens = peekable(self.tokenize(core))
        self.ntype = ntype

    def match(self, *tokens: Optional[str]):
        for token in tokens:
            assert next(self.tokens, None) == token

    def until_close(self, parse: Callable[[], T]) -> list[T]:
        parsed = []

        while self.tokens.peek() != ')':
            parsed.append(parse())

        self.match(')')

        return parsed

    def expr(self) -> Expr[Num]:
        if (tk := next(self.tokens)) == '(':
            if (tk := next(self.tokens)) == '!':
                return Annotation(self.props(), *self.until_close(self.expr))
            else:
                return Operation(tk, self.until_close(self.expr))
        elif re.match(r'[-+]?(?:\d+(?:\.\d+)?|\.\d+)(?:[eE][-+]?\d+)?$', tk):
            return Number(self.ntype(tk))
        else:
            return Symbol(tk)

    def prop(self) -> Property[Num]:
        return Property(next(self.tokens), self.expr())

    def props(self) -> list[Property[Num]]:
        props = []

        while self.tokens.peek().startswith(':'):
            props.append(self.prop())

        return props

    def fpcore(self) -> FPCore[Num]:
        self.match('(', 'FPCore')

        if self.tokens.peek() != '(':
            name = next(self.tokens)
        else:
            name = None

        self.match('(')

        args = self.until_close(lambda: next(self.tokens))
        props = self.props()
        body = self.expr()

        self.match(')', None)

        return FPCore(name, args, props, body)

    @staticmethod
    def tokenize(core: str) -> Iterator[str]:
        return filter(bool, re.split(r'([()])|\s+|;.*', core))

    @staticmethod
    def parse(core: str, ntype: Callable[[str], Num]) -> FPCore[Num]:
        return Parser(core, ntype).fpcore()
