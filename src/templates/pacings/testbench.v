`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [31:0] x1;
    reg signed [31:0] x2;
    reg signed [31:0] x3;
    reg hasX1;
    reg hasX2;
    reg hasX3;

    wire signed [31:0] result_0;
    wire signed [31:0] result_1;
    wire signed [31:0] result_2;
    wire signed [31:0] result_3;
    wire signed [31:0] result_4;
    wire signed [31:0] result_5;
    wire signed [31:0] result_6;
    wire signed [31:0] result_7;
    wire signed [31:0] result_8;
    wire signed [31:0] result_9;
    wire signed [31:0] result_10;
    wire signed [31:0] result_11;

    topEntity monitor (clk, rst, en,
                       x1, x2, x3,
                       hasX1, hasX2, hasX3,
                       result_0, result_1, result_2,
                       result_3, result_4, result_5,
                       result_6, result_7, result_8,
                       result_9, result_10, result_11
                       );

    // clock must have 100 MHz (10 ns) frequency as that has been assumed to generate enable signals
    // 10 ns = 0.01 us
    always begin
        #0.005 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        hasX1 = 0;
        hasX2 = 0;
        hasX3 = 0;
        
        $printtimescale(testbench);

        #100; // 0.1 microseconds (with timescale 1us/1ns)
        hasX1 = 1;
        hasX2 = 1;
        hasX3 = 0;
        x1 = 1;
        x2 = 2;
        x3 = 0;
        #50; // enough time for RTL data transfers
        $display("time: %0t, new (x1,x2,x3): (%b,%b,%b), (x1,x2,x3): (%0d,%0d,%0d), a = %0d, b = %0d, c = %0d, d = %0d, e = %0d, f = %0d, g = %0d, h = %0d, i = %0d, j = %0d, k = %0d, l = %0d", $time, hasX1, hasX2, hasX3, x1, x2, x3, result_0, result_1, result_2, result_3, result_4, result_5, result_6, result_7, result_8, result_9, result_10, result_11);

        #50; // 0.2 microseconds
        hasX1 = 1;
        hasX2 = 0;
        hasX3 = 0;
        x1 = 4;
        x2 = 0;
        x3 = 0;
        #50;
        $display("time: %0t, new (x1,x2,x3): (%b,%b,%b), (x1,x2,x3): (%0d,%0d,%0d), a = %0d, b = %0d, c = %0d, d = %0d, e = %0d, f = %0d, g = %0d, h = %0d, i = %0d, j = %0d, k = %0d, l = %0d", $time, hasX1, hasX2, hasX3, x1, x2, x3, result_0, result_1, result_2, result_3, result_4, result_5, result_6, result_7, result_8, result_9, result_10, result_11);

        #50;
        hasX1 = 0;
        hasX2 = 1;
        hasX3 = 0;
        x1 = 0;
        x2 = 5;
        x3 = 0;
        #50;
        $display("time: %0t, new (x1,x2,x3): (%b,%b,%b), (x1,x2,x3): (%0d,%0d,%0d), a = %0d, b = %0d, c = %0d, d = %0d, e = %0d, f = %0d, g = %0d, h = %0d, i = %0d, j = %0d, k = %0d, l = %0d", $time, hasX1, hasX2, hasX3, x1, x2, x3, result_0, result_1, result_2, result_3, result_4, result_5, result_6, result_7, result_8, result_9, result_10, result_11);
        
        #50;
        hasX1 = 0;
        hasX2 = 0;
        hasX3 = 1;
        x1 = 0;
        x2 = 0;
        x3 = 1;
        #50;
        $display("time: %0t, new (x1,x2,x3): (%b,%b,%b), (x1,x2,x3): (%0d,%0d,%0d), a = %0d, b = %0d, c = %0d, d = %0d, e = %0d, f = %0d, g = %0d, h = %0d, i = %0d, j = %0d, k = %0d, l = %0d", $time, hasX1, hasX2, hasX3, x1, x2, x3, result_0, result_1, result_2, result_3, result_4, result_5, result_6, result_7, result_8, result_9, result_10, result_11);

        #50;
        hasX1 = 1;
        hasX2 = 1;
        hasX3 = 1;
        x1 = 10;
        x2 = 10;
        x3 = 10;
        #50;
        $display("time: %0t, new (x1,x2,x3): (%b,%b,%b), (x1,x2,x3): (%0d,%0d,%0d), a = %0d, b = %0d, c = %0d, d = %0d, e = %0d, f = %0d, g = %0d, h = %0d, i = %0d, j = %0d, k = %0d, l = %0d", $time, hasX1, hasX2, hasX3, x1, x2, x3, result_0, result_1, result_2, result_3, result_4, result_5, result_6, result_7, result_8, result_9, result_10, result_11);

        #50;
        hasX1 = 1;
        hasX2 = 1;
        hasX3 = 1;
        x1 = 100;
        x2 = 100;
        x3 = 100;
        #50;
        $display("time: %0t, new (x1,x2,x3): (%b,%b,%b), (x1,x2,x3): (%0d,%0d,%0d), a = %0d, b = %0d, c = %0d, d = %0d, e = %0d, f = %0d, g = %0d, h = %0d, i = %0d, j = %0d, k = %0d, l = %0d", $time, hasX1, hasX2, hasX3, x1, x2, x3, result_0, result_1, result_2, result_3, result_4, result_5, result_6, result_7, result_8, result_9, result_10, result_11);

        #50;
        $finish; 
    end

endmodule

