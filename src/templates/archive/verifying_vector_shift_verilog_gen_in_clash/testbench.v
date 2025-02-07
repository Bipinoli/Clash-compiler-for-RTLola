`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] data;

    wire signed [63:0] mem0;
    wire signed [63:0] mem1;
    wire signed [63:0] mem2;
    wire signed [63:0] mem3;
    wire signed [63:0] mem4;
    wire signed [63:0] mem5;

    topEntity monitor (clk, rst, en,
                       data, 
                       mem0, mem1, mem2, mem3, mem4, mem5
                    );

    always begin
        #2 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #1;
        data = 1;
        #4;
        data = 2;
        #4;
        data = 3;
        #4;
        data = 4;
        #4
        data = 5;
        #4
        data = 6;
        #4
        data = 7;
        #4
        data = 8;
        #4;
        $finish;
    end

endmodule

