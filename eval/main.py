import argparse
import json
import random
import subprocess
import tempfile
from fixedpoint import FixedPoint
from pathlib import Path

import harness
from fpcore.ast import FPCore
from fpcore.rand import random_fpcore
from libm.fixed import FIXED
from libm.qformat import QFormat

def format_data(data: list[FixedPoint], fmt: QFormat) -> str:
    return json.dumps({
        'mem': {
            'data': [x.bits for x in data],
            'format': {
                'numeric_type': 'bitnum',
                'is_signed': False,
                'width': fmt.width
            }
        }
    })

def compile(fpcore: str, fmt: QFormat, nums: Path, lib: Path):
    return subprocess.run([
        nums,
        '--format', str(fmt),
        '--lib-path', lib
    ], check=True, input=fpcore, stdout=subprocess.PIPE, encoding='utf-8')

def simulate(futil: str, data: Path):
    return subprocess.run([
        'fud', 'exec',
        '--from', 'calyx',
        '--to', 'dat',
        '--through', 'icarus-verilog',
        '-s', 'verilog.data', data
    ], check=True, input=futil, stdout=subprocess.PIPE, encoding='utf-8')

def test_bench(
    benchmark: FPCore[FixedPoint],
    vals: list[FixedPoint],
    fmt: QFormat,
    nums: Path,
    lib: Path
) -> int:
    with tempfile.TemporaryDirectory() as tmp:
        data = Path(tmp, 'data.json')
        data.write_text(format_data(vals, fmt))

        bench = compile(str(benchmark), fmt, nums, lib).stdout

        name = benchmark.name or 'anonymous'
        comb = f'comb component {name}' in bench

        main = harness.single(
            name,
            comb,
            [arg.var for arg in benchmark.args],
            'mem',
            fmt.width
        )

        dat = simulate(bench + main, data).stdout

        return json.loads(dat)['memories']['mem'][0]

def format_diagnostic(
    benchmark: FPCore[FixedPoint],
    vals: list[FixedPoint],
    fmt: QFormat,
    got: FixedPoint,
    expected: FixedPoint
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
    parser = argparse.ArgumentParser(description='FPCore fuzzer.')

    parser.add_argument(
        '-e',
        '--nums-exec',
        type=Path,
        required=True,
        help='path to the calyx-nums executable'
    )
    parser.add_argument(
        '-l',
        '--lib-path',
        type=Path,
        required=True,
        help='path to the primitives library'
    )
    parser.add_argument(
        '-n',
        '--trials',
        type=int,
        default=10,
        help='number of benchmarks to generate'
    )
    parser.add_argument(
        '-f',
        '--format',
        default='UQ32.0',
        help='numeric format'
    )
    parser.add_argument(
        '--argc',
        type=int,
        default=5,
        help='number of arguments in the generated benchmarks'
    )
    parser.add_argument(
        '--size',
        type=float,
        default=25,
        help='target size for the generated benchmarks'
    )
    parser.add_argument(
        '-v',
        '--verbose',
        action='store_true',
        help='show metadata for passing tests'
    )

    args = parser.parse_args()

    fmt = QFormat.parse(args.format)
    dist = lambda: fmt.decode(random.getrandbits(fmt.width))

    print('TAP version 14')
    print(f'1..{args.trials}')

    try:
        for _ in range(args.trials):
            bench_args = [f'x{i}' for i in range(args.argc)]
            bench_vals = [dist() for _ in range(args.argc)]

            bench = random_fpcore('benchmark', bench_args, args.size, dist)

            comp = fmt.decode(test_bench(
                bench,
                bench_vals,
                fmt,
                args.nums_exec,
                args.lib_path
            ))

            interp = bench.interp(bench_vals, FIXED)

            if comp == interp and not args.verbose:
                print(f'ok - {interp:#x}')
            else:
                print('ok' if comp == interp else 'not ok')
                print(format_diagnostic(bench, bench_vals, fmt, comp, interp))
    except Exception as e:
        print('Bail out!', e)

if __name__ == '__main__':
    main()
