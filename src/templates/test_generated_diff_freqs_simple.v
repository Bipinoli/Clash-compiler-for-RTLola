`timescale 1ns/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;
    reg signed [31:0] a;

    wire signed [31:0] result_0;
    wire signed [31:0] result_1;
    wire signed [31:0] result_2;
    wire signed [31:0] result_3;

    topEntity monitor (clk, rst, en, a, result_0, result_1, result_2, result_3);

    // clock must have 100 MHz frequency as that has been assumed to generate enable signals
    // i.e #10 = 10ns = 100MHz
    always begin
        #5 clk = ~clk;
    end

    initial
    begin
        $monitor("a: %d, b: %d, c: %d, d: %d", result_0, result_1, result_2, result_3);
        // $monitor("time: %t, a: %d", $time, a);
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        a = 0;

        // 1 sec = 10^9 reps -> 0.1 s = 10^8 reps
        #100_000_000; // 0.1 sec
        rst = 0;
        a = 1;
        #100_000_000; // 0.2 sec
        a = 2;  
        #100_000_000;
        a = 3;  
        #100_000_000;
        a = 4;  
        #100_000_000;
        a = 5;  
        #100_000_000;
        a = 6;  
        #100_000_000;
        a = 7;  
        #100_000_000;
        a = 8;  
        #100_000_000;
        a = 9;  
        #100_000_000;
        a = 10;  
        #100_000_000;
        a = 11;  
        #100_000_000;
        a = 12;  
        #100_000_000;
        a = 13;  
        #100_000_000;
        $finish; 
    end

endmodule

