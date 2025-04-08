import json
import subprocess
from enum import Enum
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Any, Union

from fixedpoint import FixedPoint
from fpcorelib import FPCore

from qformat import QFormat

Args = list[FixedPoint]
Data = Union[Args, list[Args]]


class FixedPointEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, FixedPoint):
            return obj.bits

        return super().default(obj)


def format_data(memory: str, data: Data, fmt: QFormat) -> str:
    return json.dumps(
        {
            memory: {
                'data': data,
                'format': {
                    'numeric_type': 'bitnum',
                    'is_signed': False,
                    'width': fmt.width,
                },
            }
        },
        cls=FixedPointEncoder,
    )


def compile(
    fpcore: Union[FPCore[Any], Path],
    fmt: QFormat,
    nums: Path,
    lib: Path,
    args: list[str] = [],
) -> str:
    cmd = [nums, '--lib-path', lib, '--format', str(fmt), *args]

    if isinstance(fpcore, FPCore):
        stdin = str(fpcore)
    else:
        cmd.append(fpcore)
        stdin = None

    proc = subprocess.run(
        cmd, check=True, text=True, input=stdin, stdout=subprocess.PIPE
    )

    return proc.stdout


class Simulator(Enum):
    IVERILOG = 'icarus-verilog'
    VERILATOR = 'verilog'


def simulate(
    futil: str,
    data: str,
    args: list[str] = [],
    sim: Simulator = Simulator.IVERILOG,
) -> Any:
    with TemporaryDirectory() as tmp:
        file = Path(tmp, 'data.json')
        file.write_text(data)

        proc = subprocess.run([
            'fud', 'exec',
            '--from', 'calyx',
            '--to', 'dat',
            '--through', sim.value,
            '-s', 'verilog.data', file,
            *args,
        ], check=True, text=True, input=futil, stdout=subprocess.PIPE)  # fmt: skip

    return json.loads(proc.stdout)
