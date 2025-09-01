import "fpcore" as fp;
import "hls" as hls;

export const cosim_dat = state("cosim-dat", ["dat"]);

private fn cosim_setup(e) {
    e.var_("bench", "$calyx-nums-base/tools/fud/hls/cosim/bench.cpp");
    e.var_("cosim", "$calyx-nums-base/tools/fud/hls/cosim/cosim.tcl");

    e.var_("cosim-header", e.external_path(e.config_val("cosim.header")));
    e.var_("cosim-data", e.external_path(e.config_val("cosim.data")));

    e.rule("copy", "cp $in $out");

    e.build_cmd(["bench.cpp"], "copy", ["$bench"], []);
    e.build_cmd(["cosim.tcl"], "copy", ["$cosim"], []);
    e.build_cmd(["kernel.hpp"], "copy", ["$cosim-header"], []);
    e.build_cmd(["sample.dat"], "copy", ["$cosim-data"], []);

    e.var_("result", "project/solution1/sim/wrapc/result.dat");

    e.rule("vitis-cosim", "vitis_hls -f cosim.tcl > /dev/null");

    e.build_cmd(
        ["$result"],
        "vitis-cosim",
        ["cosim.tcl"],
        ["kernel.cpp", "kernel.hpp", "bench.cpp", "sample.dat"],
    );
}

op(
    "vitis-cosim",
    [fp::calyx_nums_setup, cosim_setup],
    hls::cpp_state,
    cosim_dat,
    |e, input, output| {
        e.build_cmd(["kernel.cpp"], "copy", [input], []);
        e.build_cmd([output], "copy", ["$result"], []);
    },
);
