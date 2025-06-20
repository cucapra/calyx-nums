import "calyx" as c;

export const fpcore_state = state("fpcore", ["fpcore"]);

export const calyx_nums_setup = calyx_nums_setup;

fn calyx_nums_setup(e) {
    e.config_var("calyx-nums-base", "calyx-nums.base");
    e.config_var_or(
        "calyx-nums-exe",
        "calyx-nums.exe",
        "$calyx-nums-base/target/debug/calyx-nums",
    );
    e.config_var_or("calyx-nums-args", "calyx-nums.args", "");

    e.rule(
        "calyx-nums",
        "$calyx-nums-exe -l $calyx-lib-path $calyx-nums-args $in > $out",
    );
}

op(
    "fpcore-to-calyx",
    [c::calyx_setup, calyx_nums_setup],
    fpcore_state,
    c::calyx_state,
    |e, input, output| {
        e.build_cmd([output], "calyx-nums", [input], []);
    },
);
