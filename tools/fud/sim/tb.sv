module tb;
  logic `DIM args[`ARGC];
  logic go, clk, reset;
  logic `DIM out;
  logic done;

  `DUT

  logic `DIM stimuli[`STIMULI][`ARGC];
  logic `DIM results[`STIMULI];

  string in_file, out_file;

  initial begin
    $value$plusargs("INPUT=%s", in_file);
    $readmemh(in_file, stimuli);
  end

  final begin
    $value$plusargs("OUTPUT=%s", out_file);
    $writememh(out_file, results);
  end

  task static cycle;
    repeat (2) #10 clk = ~clk;
  endtask

  longint unsigned counter, min, max, total;

  initial begin
    min = '1;
    max = 0;
    total = 0;

    go = 0;
    clk = 0;
    reset = 1;
    cycle();
    reset = 0;

    foreach (stimuli[i]) begin
      foreach (stimuli[,j])
        args[j] = stimuli[i][j];

      counter = 0;
      go = 1;

`ifndef COMB
      while (!done) begin
        cycle();
        counter++;
      end
`else
      #10;
`endif

      results[i] = out;

      min = (counter < min) ? counter : min;
      max = (counter > max) ? counter : max;
      total += counter;

      go = 0;
      cycle();
    end

    $display("min=%0d max=%0d total=%0d", min, max, total);
  end
endmodule
