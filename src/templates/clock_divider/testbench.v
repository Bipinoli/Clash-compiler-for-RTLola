`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    wire result_0;
    wire result_1;

    topEntity monitor (clk, rst, en, result_0, result_1);

    // clock must have 100 MHz (10 ns) frequency as that has been assumed to generate enable signals
    // 10 ns = 0.01 us
    always begin
        #0.005 clk = ~clk;
    end

    initial
    begin
        $printtimescale(testbench);
        $dumpvars(0, testbench);
        $monitor("clk: %b, clk1: %b, clk2: %b", clk, result_0, result_1);
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;

        #100;
        $finish;
    end

endmodule
