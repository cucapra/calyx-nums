from typing import Any, Optional

import calyx.builder as cb
from calyx.py_ast import Program, Stdlib

from fpcore.ast import FPCore
from qformat import QFormat


def single(
    comp_name: str, comb: bool, args: list[str], mem_name: str, width: int
) -> Program:
    top_level = cb.Builder()
    main = top_level.component('main')

    argc = len(args)

    mem = main.comb_mem_d1(mem_name, width, argc, argc.bit_length(), True)
    bench = main.comp_instance('bench', comp_name, False)
    run = main.group('run')

    for i, arg in enumerate(args):
        tmp = main.reg(f'arg{i}', width)
        read = main.group(f'read{i}')

        with read:
            mem.addr0 = i
            tmp.in_ = mem.read_data
            tmp.write_en = 1
            read.done = tmp.done

        with run:
            bench[arg] = tmp.out

        main.control += read

    result = main.reg('result', width)
    write = main.group('write')

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

    main.control += run
    main.control += write

    return top_level.program


def batch(
    comp_name: str,
    comb: bool,
    args: list[str],
    count: int,
    mem_name: str,
    width: int,
) -> Program:
    top_level = cb.Builder()
    main = top_level.component('main')

    argc = len(args)

    arg_width = argc.bit_length()
    idx_width = count.bit_length()

    top_level.import_("primitives/memories/comb.futil")

    mem = main.cell(
        mem_name,
        Stdlib.comb_mem_d2(width, count, argc, idx_width, arg_width),
        True,
    )

    lt = main.lt(idx_width, 'lt')
    idx = main.reg('idx', idx_width)
    init = main.group('init')
    cond = main.comb_group('cond')

    with init:
        idx.in_ = 0
        idx.write_en = 1
        init.done = idx.done

    with cond:
        lt.left = idx.out
        lt.right = count

    add = main.add(idx_width, 'add')
    inc = main.group('inc')

    with inc:
        add.left = idx.out
        add.right = 1
        idx.in_ = add.out
        idx.write_en = 1
        inc.done = idx.done

    bench = main.comp_instance('bench', comp_name, False)
    run = main.group('run')

    reads = []

    for i, arg in enumerate(args):
        tmp = main.reg(f'arg{i}', width)
        read = main.group(f'read{i}')

        with read:
            mem.addr0 = idx.out
            mem.addr1 = cb.const(arg_width, i)
            tmp.in_ = mem.read_data
            tmp.write_en = 1
            read.done = tmp.done

        with run:
            bench[arg] = tmp.out

        reads.append(read)

    result = main.reg('result', width)
    write = main.group('write')

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

    main.control += init
    main.control += cb.while_with(
        cb.CellAndGroup(lt, cond), reads + [run, write, inc]
    )

    return top_level.program


def wrap(
    fpcore: FPCore[Any],
    futil: str,
    fmt: QFormat,
    mem: str,
    count: Optional[int] = None,
) -> str:
    name = fpcore.name or 'anonymous'
    comb = f'comb component {name}' in futil

    args = [arg.var for arg in fpcore.args]

    if count is None:
        main = single(name, comb, args, mem, fmt.width)
    else:
        main = batch(name, comb, args, count, mem, fmt.width)

    imports = '\n'.join(i.doc() for i in main.imports)
    components = '\n'.join(c.doc() for c in main.components)

    return f'{imports}\n{futil}{components}'
