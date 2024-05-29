from typing import Any, Optional

import calyx.builder as cb
from calyx.py_ast import Program, Stdlib

from fpcore.ast import FPCore
from qformat import QFormat


def single(
    comp_name: str, comb: bool, args: list[str], mem_name: str, width: int
) -> Program:
    builder = cb.Builder()

    harness = builder.component('harness')
    harness.attribute('toplevel', 1)

    argc = len(args)

    mem = harness.comb_mem_d1(mem_name, width, argc, argc.bit_length(), True)
    bench = harness.comp_instance('bench', comp_name, False)
    run = harness.group('run')

    for i, arg in enumerate(args):
        tmp = harness.reg(width, f'arg{i}')
        read = harness.group(f'read{i}')

        with read:
            mem.addr0 = i
            tmp.in_ = mem.read_data
            tmp.write_en = 1
            read.done = tmp.done

        with run:
            bench[arg] = tmp.out

        harness.control += read

    result = harness.reg(width, 'result')
    write = harness.group('write')

    with run:
        if not comb:
            bench.go = ~bench.done @ cb.HI

        result.in_ = bench.out
        result.write_en = 1 if comb else bench.done
        run.done = result.done

    with write:
        mem.addr0 = 0
        mem.write_data = result.out
        mem.write_en = 1
        write.done = mem.done

    harness.control += run
    harness.control += write

    return builder.program


def batch(
    comp_name: str,
    comb: bool,
    args: list[str],
    count: int,
    mem_name: str,
    width: int,
) -> Program:
    builder = cb.Builder()

    harness = builder.component('harness')
    harness.attribute('toplevel', 1)

    argc = len(args)

    arg_width = argc.bit_length()
    idx_width = count.bit_length()

    builder.import_('primitives/memories/comb.futil')

    mem = harness.cell(
        mem_name,
        Stdlib.comb_mem_d2(width, count, argc, idx_width, arg_width),
        True,
    )

    lt = harness.lt(idx_width, 'lt')
    idx = harness.reg(idx_width, 'idx')
    init = harness.group('init')
    cond = harness.comb_group('cond')

    with init:
        idx.in_ = 0
        idx.write_en = 1
        init.done = idx.done

    with cond:
        lt.left = idx.out
        lt.right = count

    add = harness.add(idx_width, 'add')
    inc = harness.group('inc')

    with inc:
        add.left = idx.out
        add.right = 1
        idx.in_ = add.out
        idx.write_en = 1
        inc.done = idx.done

    bench = harness.comp_instance('bench', comp_name, False)
    run = harness.group('run')

    reads = []

    for i, arg in enumerate(args):
        tmp = harness.reg(width, f'arg{i}')
        read = harness.group(f'read{i}')

        with read:
            mem.addr0 = idx.out
            mem.addr1 = cb.const(arg_width, i)
            tmp.in_ = mem.read_data
            tmp.write_en = 1
            read.done = tmp.done

        with run:
            bench[arg] = tmp.out

        reads.append(read)

    result = harness.reg(width, 'result')
    write = harness.group('write')

    with run:
        if not comb:
            bench.go = ~bench.done @ cb.HI

        result.in_ = bench.out
        result.write_en = 1 if comb else bench.done
        run.done = result.done

    with write:
        mem.addr0 = idx.out
        mem.addr1 = cb.const(arg_width, 0)
        mem.write_data = result.out
        mem.write_en = cb.HI
        write.done = mem.done

    harness.control += init
    harness.control += cb.while_with(
        cb.CellAndGroup(lt, cond), reads + [run, write, inc]
    )

    return builder.program


def wrap(
    fpcore: FPCore[Any],
    futil: str,
    fmt: QFormat,
    mem: str,
    count: Optional[int] = None,
) -> str:
    name = fpcore.name or 'main'
    comb = f'comb component {name}' in futil

    args = [arg.var for arg in fpcore.args]

    if count is None:
        main = single(name, comb, args, mem, fmt.width)
    else:
        main = batch(name, comb, args, count, mem, fmt.width)

    imports = '\n'.join(i.doc() for i in main.imports)
    components = '\n'.join(c.doc() for c in main.components)

    return f'{imports}\n{futil}{components}'
