open_project -reset project

add_files -cflags "-std=c++11" kernel.cpp
add_files -tb -cflags "-std=c++11" "bench.cpp sample.dat"
set_top ex0

open_solution -reset solution1

set_part "xczu3eg-sbva484-1-e"
create_clock -period 7

csynth_design
cosim_design

exit
