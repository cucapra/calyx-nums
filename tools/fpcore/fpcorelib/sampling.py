import math
import random
from collections.abc import Generator
from dataclasses import dataclass
from enum import Enum
from typing import Generic, Protocol, SupportsFloat, TypeVar

from more_itertools import pairwise

from .ast import Data, FPCore, Number, Operation, Symbol


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
            self.GT: self.GT,
        }[self]

    def rev(self):
        return {
            self.LT: self.GT,
            self.LE: self.GE,
            self.GE: self.LE,
            self.GT: self.LT,
        }[self]


Num = TypeVar('Num', bound=Comparable)


@dataclass
class Interval(Generic[Num]):
    left: Num
    right: Num

    def constrain(self, rel: Rel, rhs: Num):
        if rel.strict() == Rel.LT:
            self.right = min(self.right, rhs)
        else:
            self.left = max(self.left, rhs)


def parse_precondition(pre: Data[Num], min: Num, max: Num):
    intervals: dict[str, Interval[Num]] = {}

    def walk(expr: Data[Num]):
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

                intervals[left.id].constrain(rel, right.val)

    walk(pre)

    return intervals


def parse_domain(
    fpcore: FPCore[Num], min: Num, max: Num
) -> list[Interval[Num]]:
    for prop in fpcore.props:
        if prop.name == ':pre':
            domains = parse_precondition(prop.data, min, max)

            return [domains[arg.var] for arg in fpcore.args]

    raise RuntimeError('missing precondition')


def sample(domain: Interval[float]) -> float:
    while True:
        value = random.uniform(domain.left, domain.right)

        if domain.left < value < domain.right:
            return value


def sample_domain(fpcore: FPCore[float], n: int) -> Generator[list[float]]:
    domain = parse_domain(fpcore, -math.inf, math.inf)

    return (list(map(sample, domain)) for _ in range(n))
