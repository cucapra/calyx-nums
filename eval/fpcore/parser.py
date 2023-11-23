import re
from collections.abc import Callable, Iterator
from typing import Generic, Optional, TypeVar

from more_itertools import peekable

from .ast import (
    Annotation,
    Argument,
    Binder,
    Expr,
    FPCore,
    Let,
    Num,
    Number,
    Operation,
    Property,
    Symbol,
)

T = TypeVar('T')


class Parser(Generic[Num]):
    def __init__(self, core: str, ntype: Callable[[str], Num]):
        self.tokens = peekable(self.tokenize(core))
        self.ntype = ntype

    def peek(self) -> str:
        return self.tokens.peek()

    def next(self) -> str:
        return next(self.tokens)

    def match(self, *tokens: Optional[str]):
        for token in tokens:
            assert next(self.tokens, None) == token

    def until_close(self, parse: Callable[[], T]) -> list[T]:
        parsed = []

        while self.peek() != ')':
            parsed.append(parse())

        self.match(')')

        return parsed

    def expr(self) -> Expr[Num]:
        if (tk := self.next()) == '(':
            if (tk := self.next()) == '!':
                props = self.props()
                body = self.expr()

                self.match(')')

                return Annotation(props, body)
            elif tk in ('let', 'let*'):
                self.match('(')

                binders = self.until_close(self.binder)
                body = self.expr()

                self.match(')')

                return Let(binders, body, tk[-1] == '*')
            else:
                return Operation(tk, self.until_close(self.expr))
        elif re.match(r'[-+]?(?:\d+(?:\.\d+)?|\.\d+)(?:[eE][-+]?\d+)?$', tk):
            return Number(self.ntype(tk))
        else:
            return Symbol(tk)

    def binder(self) -> Binder[Num]:
        self.match('[')

        var = self.next()
        expr = self.expr()

        self.match(']')

        return Binder(var, expr)

    def prop(self) -> Property[Num]:
        return Property(self.next(), self.expr())

    def props(self) -> list[Property[Num]]:
        props = []

        while self.peek().startswith(':'):
            props.append(self.prop())

        return props

    def arg(self) -> Argument[Num]:
        if (tk := self.next()) == '(':
            self.match('!')

            props = self.props()
            var = self.next()

            self.match(')')

            return Argument(var, props)
        else:
            return Argument(tk, [])

    def fpcore(self) -> FPCore[Num]:
        self.match('(', 'FPCore')

        if self.peek() != '(':
            name = self.next()
        else:
            name = None

        self.match('(')

        args = self.until_close(self.arg)
        props = self.props()
        body = self.expr()

        self.match(')', None)

        return FPCore(name, args, props, body)

    @staticmethod
    def tokenize(core: str) -> Iterator[str]:
        subs = re.split(r'([][)(]|"[^"]*")|\s+|;.*', core)

        return filter(bool, subs)

    @staticmethod
    def parse(core: str, ntype: Callable[[str], Num]) -> FPCore[Num]:
        return Parser(core, ntype).fpcore()
