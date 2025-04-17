import operator
from collections.abc import Callable
from itertools import combinations
from typing import TypeVar

from more_itertools import pairwise

Num = TypeVar('Num')


def all_windows(f: Callable[[Num, Num], bool]):
    def variadic(*args: Num):
        return all(f(*pair) for pair in pairwise(args))

    return variadic


def all_combinations(f: Callable[[Num, Num], bool]):
    def variadic(*args: Num):
        return all(f(*pair) for pair in combinations(args, 2))

    return variadic


lt = all_windows(operator.lt)
gt = all_windows(operator.gt)
le = all_windows(operator.le)
ge = all_windows(operator.ge)
eq = all_windows(operator.eq)
ne = all_combinations(operator.ne)


def and_(*args: object):
    return all(args)


def or_(*args: object):
    return any(args)
