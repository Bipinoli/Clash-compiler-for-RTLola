`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [31:0] x1;
    reg signed [31:0] x2;

    reg newX1;
    reg newX2;

    wire signed [31:0] result_0;
    wire signed [31:0] result_1_0;
    wire signed [31:0] result_1_1;
    wire signed [31:0] result_1_2;

    topEntity monitor (clk, rst, en,
                       x1, x2,
                       newX1, newX2,
                       result_0,
                       result_1_0, result_1_1, result_1_2
                       );

    // clock must have 100 MHz (10 ns) frequency as that new been assumed to generate enable signals
    // 10 ns = 0.01 us
    always begin
        #0.005 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        newX1 = 0;
        newX2 = 0;
        
        $printtimescale(testbench);

        #100; // 0.1 microseconds (with timescale 1us/1ns)
        newX1 = 1;
        newX2 = 1;
        x1 = 1;
        x2 = 2;
        #0.01; // data is there only for an instant -> one clock 
        newX1 = 0;
        newX2 = 0;
        #49.99; // enough time for RTL data transfers
        $display("time: %0t, (x1,x2): (%0d,%0d), a = %0d, b = (%0d, %0d, %0d)", $time, x1, x2, result_0, result_1_0, result_1_1, result_1_2);

        #50; // 0.2 microseconds
        newX1 = 1;
        newX2 = 0;
        x1 = 4;
        x2 = 0;
        #0.01; // data available only for 1 clock cycle
        newX1 = 0;
        newX2 = 0;
        #50;
        $display("time: %0t, (x1,x2): (%0d,%0d), a = %0d, b = (%0d, %0d, %0d)", $time, x1, x2, result_0, result_1_0, result_1_1, result_1_2);

        #50;
        newX1 = 0;
        newX2 = 1;
        x1 = 0;
        x2 = 5;
        #0.01; // data available only for 1 clock cycle
        newX1 = 0;
        newX2 = 0;
        #50;
        $display("time: %0t, (x1,x2): (%0d,%0d), a = %0d, b = (%0d, %0d, %0d)", $time, x1, x2, result_0, result_1_0, result_1_1, result_1_2);
        
        #50;
        newX1 = 0;
        newX2 = 0;
        x1 = 0;
        x2 = 0;
        #0.01; // data available only for 1 clock cycle
        newX1 = 0;
        newX2 = 0;
        #50;
        $display("time: %0t, (x1,x2): (%0d,%0d), a = %0d, b = (%0d, %0d, %0d)", $time, x1, x2, result_0, result_1_0, result_1_1, result_1_2);

        #50;
        newX1 = 1;
        newX2 = 1;
        x1 = 10;
        x2 = 10;
        #0.01; // data available only for 1 clock cycle
        newX1 = 0;
        newX2 = 0;
        #50;
        $display("time: %0t, (x1,x2): (%0d,%0d), a = %0d, b = (%0d, %0d, %0d)", $time, x1, x2, result_0, result_1_0, result_1_1, result_1_2);

        #50;
        newX1 = 1;
        newX2 = 1;
        x1 = 100;
        x2 = 100;
        #0.01; // data available only for 1 clock cycle
        newX1 = 0;
        newX2 = 0;
        #50;
        $display("time: %0t, new (x1,x2): (%b,%b), (x1,x2): (%0d,%0d), a = %0d, b = (%0d, %0d, %0d)", $time, newX1, newX2, x1, x2, result_0, result_1_0, result_1_1, result_1_2);
        $display("time: %0t, (x1,x2): (%0d,%0d), a = %0d, b = (%0d, %0d, %0d)", $time, x1, x2, result_0, result_1_0, result_1_1, result_1_2);

        #50;
        $finish; 
    end

endmodule

