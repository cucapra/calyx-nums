from collections.abc import Callable
from typing import Generic

from lark import Lark, Transformer, v_args

from . import ast


class FPCoreTransformer(Transformer[ast.FPCore[ast.Num]]):
    def __init__(self, n_type: Callable[[str], ast.Num]):
        super().__init__(False)

        self.n_type = n_type

    fpcore = v_args(True)(ast.FPCore)

    @staticmethod
    def argument(children):
        *props, var = children

        return ast.Argument(var, props)

    @staticmethod
    def operation(children):
        op, *args = children

        return ast.Operation(op, args)

    @staticmethod
    def let(children):
        keyword, *binders, body = children

        return ast.Let(binders, body, keyword.endswith('*'))

    annotation = v_args(True)(ast.Annotation)
    binder = v_args(True)(ast.Binder)

    def number(self, children):
        return ast.Number(self.n_type(*children))

    symbol = v_args(True)(ast.Symbol)
    property = v_args(True)(ast.Property)

    list = list


class FPCoreParser(Generic[ast.Num]):
    def __init__(self, n_type: Callable[[str], ast.Num]):
        self.parser = Lark.open(
            'syntax.lark',
            rel_to=__file__,
            parser='lalr',
            transformer=FPCoreTransformer(n_type),
        )

    def parse(self, text: str) -> ast.FPCore[ast.Num]:
        return self.parser.parse(text)  # type: ignore
