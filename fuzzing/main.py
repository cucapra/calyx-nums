import argparse
import json
import random
import subprocess
import tempfile
from pathlib import Path
from typing import Any

from lib.fpcore import FPCore, random_fpcore
from lib.harness import harness

def format_data(data: list[int], width: int):
    return json.dumps({
        'mem': {
            'data': data,
            'format': {
                'numeric_type': 'bitnum',
                'is_signed': False,
                'width': width
            }
        }
    })

def compile(file: Path, width: int, nums: Path, lib: Path):
    return subprocess.run([
        nums,
        file,
        '--width', str(width),
        '--lib-path', lib
    ], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, encoding='utf-8')

def simulate(futil: str, data: Path):
    return subprocess.run([
        'fud', 'exec',
        '--from', 'calyx',
        '--to', 'dat',
        '--through', 'icarus-verilog',
        '-s', 'verilog.data', data
    ], stdout=subprocess.PIPE, input=futil, encoding='utf-8')

def test_bench(
    benchmark: FPCore,
    args: list[str],
    vals: list[int],
    width: int,
    nums: Path,
    lib: Path
):
    with tempfile.TemporaryDirectory() as tmp:
        core = Path(tmp, 'bench.fpcore')
        data = Path(tmp, 'dat.json')

        core.write_text(str(benchmark))
        data.write_text(format_data(vals, width))

        comp_result = compile(core, width, nums, lib)
        comp_result.check_returncode()

        bench = comp_result.stdout
        main = harness('anonymous', args, width)

        sim_result = simulate(f'{bench}\n{main}', data)
        sim_result.check_returncode()

        result = json.loads(sim_result.stdout)

        return result['memories']['mem'][0]

def format_diagnostic(benchmark: FPCore, got: Any, expected: int):
    return (
        '  ---\n'
        '  FPCore: {core}\n'
        '  got: {got}\n'
        '  expected: {expected}\n'
        '  ...'
    ).format(core=benchmark, got=got, expected=expected)

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
    parser.add_argument('--width', type=int, default=32, help='word size')
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
        '--lit-max',
        type=int,
        default=64,
        help='maximum value for generated literals'
    )
    parser.add_argument(
        '-v',
        '--verbose',
        action='store_true',
        help='show metadata for passing tests'
    )

    args = parser.parse_args()

    print('TAP version 14')
    print(f'1..{args.trials}')

    for _ in range(args.trials):
        bench_args = [f'x{i}' for i in range(args.argc)]
        bench_vals = [random.randint(0, args.lit_max) for _ in range(args.argc)]

        bench = random_fpcore(bench_args, args.size, args.lit_max, args.width)

        comp = test_bench(
            bench,
            bench_args,
            bench_vals,
            args.width,
            args.nums_exec,
            args.lib_path
        )
        interp = bench.interp(bench_vals)

        if comp == interp and not args.verbose:
            print(f'ok - {interp}')
        else:
            print('ok' if comp == interp else 'not ok')
            print(format_diagnostic(bench, comp, interp))

if __name__ == '__main__':
    main()
