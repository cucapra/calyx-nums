import re
import sys
from collections.abc import Iterable
from dataclasses import dataclass


@dataclass
class Port:
    direction: str
    packed_dimension: str
    identifier: str


def ports(sv: str, module: str):
    if match := re.search(rf'module {module}\(([^)]*)\)', sv):
        ports = match.group(1)

        for match in re.finditer(
            r'(input|output) logic (\[[^]]*\])? ?(\w+)', ports
        ):
            yield Port(*match.groups(default=''))
    else:
        raise RuntimeError(f"couldn't find module `{module}`")


class Module:
    identifier: str
    inputs: list[Port]
    outputs: list[Port]
    interface: list[Port]

    def __init__(self, identifier: str, ports: Iterable[Port]):
        self.identifier = identifier
        self.inputs = []
        self.outputs = []
        self.interface = []

        for port in ports:
            if port.identifier in ('go', 'clk', 'reset', 'done'):
                self.interface.append(port)
            elif port.direction == 'input':
                self.inputs.append(port)
            else:
                self.outputs.append(port)

    @classmethod
    def parse(cls, sv: str, module: str):
        return cls(module, ports(sv, module))

    def instantiate(self):
        connections = ', '.join(
            f'.{port.identifier}(args[{i}])'
            for i, port in enumerate(self.inputs)
        )

        return f'{self.identifier} dut ({connections}, .*);'


def emit_macros(sv: str, dat: str, out: str):
    with open(sv, 'r') as f:
        sv = f.read()

    dut = Module.parse(sv, 'main')

    with open(dat, 'r') as f:
        stimuli = sum(1 for line in f if not line.isspace())

    with open(out, 'w') as f:
        f.write(
            f'`define DUT {dut.instantiate()}\n'
            f'`define DIM {dut.inputs[0].packed_dimension}\n'
            f'`define ARGC {len(dut.inputs)}\n'
            f'`define STIMULI {stimuli}\n'
        )

        if all(port.identifier != 'done' for port in dut.interface):
            f.write('`define COMB\n')


if __name__ == '__main__':
    emit_macros(*sys.argv[1:])
