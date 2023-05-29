import calyx.builder as cb

def harness(component_name: str, args: list[str], width: int) -> str:
    top_level = cb.Builder()
    main = top_level.component('main')

    mem = main.mem_d1('mem', width, len(args), len(args).bit_length(), True)
    bench = main.comp_instance('bench', component_name, False)
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

        main.control += read.as_enable()

    with write:
        bench.go = cb.const(1, 1)

        mem.addr0 = 0
        mem.write_data = bench.out
        mem.write_en = bench.done @ 1
        write.done = mem.done

    main.control += write.as_enable()

    top_level.program.imports = []  # Suppress imports

    return top_level.program.doc()
