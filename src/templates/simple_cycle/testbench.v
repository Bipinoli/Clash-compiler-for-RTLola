`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [31:0] x;
    reg newX;

    wire signed [31:0] result_0;
    wire signed [31:0] result_1;
    wire signed [31:0] result_2_0;
    wire signed [31:0] result_2_1;
    wire signed [31:0] result_2_2;

    topEntity monitor (clk, rst, en,
                       x, newX,
                       result_0, result_1,
                       result_2_0, result_2_1, result_2_2
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
        newX = 0;
        
        $printtimescale(testbench);

        #100; // 0.1 microseconds (with timescale 1us/1ns)
        newX = 1;
        x = 1;
        #0.01; // data is there only for an instant -> one clock 
        newX = 0;
        #49.99; // enough time for RTL data transfers
        $display("time: %0t, x: %0d, a: %0d, b: %0d, c: (%0d, %0d, %0d)", $time, x, result_0, result_1, result_2_0, result_2_1, result_2_2);

        #50; // 0.2 microseconds
        newX = 1;
        x = 2;
        #0.01; // data available only for 1 clock cycle
        newX = 0;
        #49.99;
        $display("time: %0t, x: %0d, a: %0d, b: %0d, c: (%0d, %0d, %0d)", $time, x, result_0, result_1, result_2_0, result_2_1, result_2_2);

        #50;
        newX = 1;
        x = 3;
        #0.01; // data available only for 1 clock cycle
        newX = 0;
        #49.99;
        $display("time: %0t, x: %0d, a: %0d, b: %0d, c: (%0d, %0d, %0d)", $time, x, result_0, result_1, result_2_0, result_2_1, result_2_2);
         
        #50;
        newX = 1;
        x = 4;
        #0.01; // data available only for 1 clock cycle
        newX = 0;
        #49.99;
        $display("time: %0t, x: %0d, a: %0d, b: %0d, c: (%0d, %0d, %0d)", $time, x, result_0, result_1, result_2_0, result_2_1, result_2_2);

        #50;
        newX = 1;
        x = 5;
        #0.01; // data available only for 1 clock cycle
        newX = 0;
        #49.99;
        $display("time: %0t, x: %0d, a: %0d, b: %0d, c: (%0d, %0d, %0d)", $time, x, result_0, result_1, result_2_0, result_2_1, result_2_2);
        
        #50;
        newX = 1;
        x = 6;
        #0.01; // data available only for 1 clock cycle
        newX = 0;
        #49.99;
        $display("time: %0t, x: %0d, a: %0d, b: %0d, c: (%0d, %0d, %0d)", $time, x, result_0, result_1, result_2_0, result_2_1, result_2_2);
        
        #50;
        $finish; 
    end

endmodule

