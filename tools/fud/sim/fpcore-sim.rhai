import "calyx" as c;
import "fpcore" as fp;

export const fpcore_dat = state("fpcore-dat", ["dat"]);
export const fpcore_sim = state("fpcore-sim", []);

private fn data_setup(e) {
    e.var_("sim-data", e.external_path(e.config_val("sim.data")));
}

private fn tb_setup(e) {
    e.var_("tb", "$calyx-libm-base/tools/fud/sim/tb.sv");
    e.var_("macros", "$calyx-libm-base/tools/fud/sim/macros.py");

    e.rule("tb-macros", "python3 $macros $in $out");
}

private fn tb_build(e, input) {
    e.build_cmd(
        ["macros.sv"],
        "tb-macros",
        [input, "$sim-data"],
        [],
    );
}

private fn verilator_setup(e) {
    e.rule(
        "verilator-compile-fpcore-tb",
        "verilator --binary --top tb --Mdir obj_dir -o ../$out $in",
    );
}

op(
    "fpcore-verilator",
    [fp::calyx_libm_setup, data_setup, tb_setup, verilator_setup],
    c::verilog_state,
    fpcore_sim,
    |e, input, output| {
        tb_build(e, input);

        e.build_cmd(
            [output],
            "verilator-compile-fpcore-tb",
            ["macros.sv", "$tb", input],
            [],
        );
    },
);

private fn icarus_setup(e) {
    e.rule(
        "icarus-compile-fpcore-tb",
        "iverilog -g2012 -s tb -o $out $in",
    );
}

op(
    "fpcore-icarus",
    [fp::calyx_libm_setup, data_setup, tb_setup, icarus_setup],
    c::verilog_noverify,
    fpcore_sim,
    |e, input, output| {
        tb_build(e, input);

        e.build_cmd(
            [output],
            "icarus-compile-fpcore-tb",
            ["macros.sv", "$tb", input],
            [],
        );
    },
);

private fn sim_setup(e) {
    e.var_("extract", "$calyx-libm-base/tools/fud/sim/extract.py");

    e.rule(
        "run-fpcore-sim",
        "./$in +INPUT=$sim-data +OUTPUT=results.out > sim.log",
    );

    e.rule("extract-fpcore-dat", "python3 $extract $in $out");
}

op(
    "fpcore-simulate",
    [fp::calyx_libm_setup, data_setup, sim_setup],
    fpcore_sim,
    fpcore_dat,
    |e, input, output| {
        e.build_cmd(
            ["results.out", "sim.log"],
            "run-fpcore-sim",
            [input],
            ["$sim-data"],
        );

        e.build_cmd(
            [output],
            "extract-fpcore-dat",
            ["results.out", "sim.log"],
            [],
        );
    },
);
