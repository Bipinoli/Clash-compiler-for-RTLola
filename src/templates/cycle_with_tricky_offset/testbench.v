`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] x;
    reg newX;

    wire outputPhase;
    wire signed [63:0] a;
    wire signed [63:0] b;
    wire signed [63:0] c;
    wire enA;
    wire enB;
    wire enC;

    topEntity monitor (clk, rst, en,
                       x, newX,
                       outputPhase,
                       a, enA,
                       b, enB,
                       c, enC
                       );

    // clock must have 100 MHz (10 ns) frequency as that has been assumed to generate enable signals
    // 10 ns = 0.01 us
    always begin
        #0.005 clk = ~clk;
    end

    // show output when the machine is in output phase
    always @(outputPhase) begin
        $display("time: %0t, output: %0d, (x, newX): (%0d, %0d), ready: (%0d, %0d, %0d), a: %0d, b: %0d, c: %0d",
         $time, outputPhase, x, newX, enA, enB, enC, a, b, c);
    end

    // LLC runs 4X faster than HLC so the whole machine runs at 1/4th of the clock frequency
    // 1 clock cycle = #0.01
    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        newX = 0;
        
        $printtimescale(testbench);

        #0.04;
        x = 1;
        newX = 1;
        
        #0.04;
        x = 2;

        #0.04;
        x = 3;

        #0.04;
        x = 4;

        #0.04;
        x = 5;

        #0.04;
        x = 6;

        #0.04;
        $finish; 
    end

endmodule

