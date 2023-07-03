import calyx.builder as cb

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
