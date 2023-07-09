import math
import random
from dataclasses import dataclass
from enum import Enum
from more_itertools import pairwise
from typing import Generic, Protocol, SupportsFloat, TypeVar

from .ast import Expr, FPCore, Number, Operation, Symbol

class Comparable(SupportsFloat, Protocol):
    def __lt__(self, other, /) -> bool: ...

class Rel(Enum):
    LT = '<'
    LE = '<='
    GE = '>='
    GT = '>'

    def strict(self):
        return {
            self.LT: self.LT,
            self.LE: self.LT,
            self.GE: self.GT,
            self.GT: self.GT
        }[self]

    def rev(self):
        return {
            self.LT: self.GT,
            self.LE: self.GE,
            self.GE: self.LE,
            self.GT: self.LT
        }[self]

Num = TypeVar('Num', bound=Comparable)

@dataclass
class Interval(Generic[Num]):
    left: Num
    right: Num

    def intersect(self, rel: Rel, rhs: Num):
        if rel.strict() == Rel.LT:
            self.right = min(self.right, rhs)
        else:
            self.left = max(self.left, rhs)

def domain(pre: Expr[Num], min: Num, max: Num):
    intervals: dict[str, Interval[Num]] = {}

    def walk(expr: Expr[Num]):
        assert isinstance(expr, Operation)

        if expr.op == 'and':
            for arg in expr.args:
                walk(arg)
        else:
            for left, right in pairwise(expr.args):
                rel = Rel(expr.op)

                if not isinstance(left, Symbol):
                    left, right = right, left
                    rel = rel.rev()

                assert isinstance(left, Symbol)
                assert isinstance(right, Number)

                if left.id not in intervals:
                    intervals[left.id] = Interval(min, max)

                intervals[left.id].intersect(rel, right.val)

    walk(pre)

    return intervals

def sample(domain: Interval[float]) -> float:
    while True:
        value = random.uniform(domain.left, domain.right)

        if domain.left < value < domain.right:
            return value

def sample_args(bench: FPCore[float], n: int) -> list[list[float]]:
    for prop in bench.props:
        if prop.name == ':pre':
            domains = domain(prop.data, -math.inf, math.inf)
            break
    else:
        raise RuntimeError('No domain specified')

    return [[sample(domains[arg]) for arg in bench.args] for _ in range(n)]
