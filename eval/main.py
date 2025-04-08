import random
from argparse import ArgumentParser
from pathlib import Path

from fixedpoint import FixedPoint
from fpcorelib import FPCore
from fpcorelib.interp.fixedpoint import LIB_FIXED_POINT
from fpcorelib.rand import random_fpcore

from qformat import QFormat, RoundingMode
from sim import harness
from sim import run as runner


def run(
    benchmark: FPCore[FixedPoint],
    vals: list[FixedPoint],
    fmt: QFormat,
    nums: Path,
    lib: Path,
) -> int:
    compiled = runner.compile(benchmark, fmt, nums, lib)

    prog = harness.wrap(benchmark, compiled, fmt, 'mem')
    data = runner.format_data('mem', vals, fmt)

    result = runner.simulate(prog, data)

    return result['memories']['mem'][0]


def format_diagnostic(
    benchmark: FPCore[FixedPoint],
    vals: list[FixedPoint],
    fmt: QFormat,
    got: FixedPoint,
    expected: FixedPoint,
) -> str:
    args = ', '.join(map(hex, vals))

    return (
        '  ---\n'
        '  FPCore: {core}\n'
        '  args: [{args}]\n'
        '  format: {fmt}\n'
        '  got: {got:#x}\n'
        '  expected: {expected:#x}\n'
        '  ...'
    ).format(core=benchmark, args=args, fmt=fmt, got=got, expected=expected)


def main():
    parser = ArgumentParser(description='FPCore fuzzer.')

    parser.add_argument(
        '-e',
        '--nums-exec',
        type=Path,
        required=True,
        help='path to the calyx-nums executable',
    )
    parser.add_argument(
        '-l',
        '--lib-path',
        type=Path,
        required=True,
        help='path to the primitives library',
    )
    parser.add_argument(
        '-n',
        '--trials',
        type=int,
        default=10,
        help='number of benchmarks to generate',
    )
    parser.add_argument(
        '-f',
        '--format',
        default='UQ32.0',
        help='numeric format',
    )
    parser.add_argument(
        '--argc',
        type=int,
        default=5,
        help='number of arguments in the generated benchmarks',
    )
    parser.add_argument(
        '--size',
        type=float,
        default=25,
        help='target size for the generated benchmarks',
    )
    parser.add_argument(
        '-v',
        '--verbose',
        action='store_true',
        help='show metadata for passing tests',
    )

    opts = parser.parse_args()

    fmt = QFormat.parse(opts.format)

    mode: RoundingMode = {
        'overflow': 'wrap',
        'rounding': 'down',
        'overflow_alert': 'ignore',
    }

    def dist():
        return fmt.decode(random.getrandbits(fmt.width), mode)

    print('TAP version 14')
    print(f'1..{opts.trials}')

    try:
        for _ in range(opts.trials):
            args = [f'x{i}' for i in range(opts.argc)]
            vals = [dist() for _ in range(opts.argc)]

            bench = random_fpcore('benchmark', args, opts.size, dist)

            got = fmt.decode(
                run(
                    bench,
                    vals,
                    fmt,
                    opts.nums_exec,
                    opts.lib_path,
                )
            )

            expected = bench.interp(vals, LIB_FIXED_POINT)

            if got == expected and not opts.verbose:
                print(f'ok - {got:#x}')
            else:
                print('ok' if got == expected else 'not ok')
                print(format_diagnostic(bench, vals, fmt, got, expected))
    except Exception as e:
        print('Bail out!', e)


if __name__ == '__main__':
    main()
