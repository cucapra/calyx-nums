import "fpcore" as fp;
import "hls" as hls;

fn fpbench_setup(e) {
    e.config_var("fpbench-exporter", "fpbench.exporter");
    e.config_var_or("fpbench-lang", "fpbench.lang", "vivado");

    e.rule(
        "fpbench-export",
        "racket $fpbench-exporter --lang $fpbench-lang $in $out",
    );
}

op(
    "fpbench-exporter",
    [fpbench_setup],
    fp::fpcore_state,
    hls::cpp_state,
    |e, input, output| {
        e.build_cmd([output], "fpbench-export", [input], []);
    },
);
