from typing import Any, Generic

from .ast import Annotation, Expr, If, Let, Num, Number, Operation, Symbol


class Visitor(Generic[Num]):
    def visit_sym(self, node: Symbol[Num]) -> Any:
        pass

    def visit_num(self, node: Number[Num]) -> Any:
        pass

    def visit_op(self, node: Operation[Num]) -> Any:
        for arg in node.args:
            self.visit(arg)

    def visit_if(self, node: If[Num]) -> Any:
        self.visit(node.cond)
        self.visit(node.true)
        self.visit(node.false)

    def visit_let(self, node: Let[Num]) -> Any:
        for binder in node.binders:
            self.visit(binder.expr)

        self.visit(node.body)

    def visit_bang(self, node: Annotation[Num]) -> Any:
        self.visit(node.body)

    def visit(self, node: Expr[Num]) -> Any:
        visitor = {
            'Symbol': self.visit_sym,
            'Number': self.visit_num,
            'Operation': self.visit_op,
            'If': self.visit_if,
            'Let': self.visit_let,
            'Annotation': self.visit_bang,
        }[node.__class__.__name__]

        return visitor(node)
