`timescale 1us/1ns
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

    // clock must have 100 MHz (10 ns) frequency as that has been assumed to generate enable signals
    // 10 ns = 0.01 us
    always begin
        #0.005 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        a = 0;
        
        $printtimescale(testbench);

        #100; // 0.1 microseconds (with timescale 1us/1ns)
        rst = 0;
        a = 1;
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 2;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 3;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 4;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 5;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 6;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 7;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 8;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 9;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 10;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 11;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 12;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);

        #100;
        a = 13;  
        $display("time: %t, a: %d, b: %d, c: %d, d: %d", $time, result_0, result_1, result_2, result_3);
        
        #100;
        $finish; 
    end

endmodule

