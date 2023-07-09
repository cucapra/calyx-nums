import calyx.builder as cb
from calyx.py_ast import Stdlib

def single(
    comp_name: str, comb: bool, args: list[str], mem_name: str, width: int
) -> str:
    top_level = cb.Builder()
    main = top_level.component('main')

    argc = len(args)

    mem = main.mem_d1(mem_name, width, argc, argc.bit_length(), True)
    bench = main.comp_instance('bench', comp_name, False)
    write = main.group('write')

    for i, arg in enumerate(args):
        tmp = main.reg(f'tmp{i}', width)
        read = main.group(f'read{i}')

        with read:
            mem.addr0 = i
            tmp.in_ = mem.read_data
            tmp.write_en = 1
            read.done = tmp.done

        with write:
            bench[arg] = tmp.out

        main.control += read

    with write:
        if not comb:
            bench.go = cb.HI

        mem.addr0 = 0
        mem.write_data = bench.out
        mem.write_en = 1 if comb else bench.done @ 1
        write.done = mem.done

    main.control += write

    top_level.program.imports = []  # Suppress imports

    return top_level.program.doc()

def batch(
    comp_name: str,
    comb: bool,
    args: list[str],
    count: int,
    mem_name: str,
    width: int
) -> str:
    top_level = cb.Builder()
    main = top_level.component('main')

    argc = len(args)

    arg_width = argc.bit_length()
    idx_width = count.bit_length()

    mem = main.cell(
        mem_name,
        Stdlib.mem_d2(
            width,
            count,
            argc,
            idx_width,
            arg_width
        ),
        True
    )

    lt = main.lt('lt', idx_width)
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

    add = main.add('add', idx_width)
    inc = main.group('inc')

    with inc:
        add.left = idx.out
        add.right = 1
        idx.in_ = add.out
        idx.write_en = 1
        inc.done = idx.done

    bench = main.comp_instance('bench', comp_name, False)
    write = main.group('write')

    reads = []

    for i, arg in enumerate(args):
        tmp = main.reg(f'tmp{i}', width)
        read = main.group(f'read{i}')

        with read:
            mem.addr0 = idx.out
            mem.addr1 = cb.const(arg_width, i)
            tmp.in_ = mem.read_data
            tmp.write_en = 1
            read.done = tmp.done

        with write:
            bench[arg] = tmp.out

        reads.append(read)

    with write:
        if not comb:
            bench.go = cb.HI

        mem.addr0 = idx.out
        mem.addr1 = cb.const(arg_width, 0)
        mem.write_data = bench.out
        mem.write_en = cb.HI if comb else bench.done @ cb.HI
        write.done = mem.done

    main.control += init
    main.control += cb.while_(lt.out, cond, reads + [write, inc])

    top_level.program.imports = []

    return top_level.program.doc()
