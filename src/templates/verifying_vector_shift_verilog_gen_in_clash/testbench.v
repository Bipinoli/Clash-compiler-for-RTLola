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

    topEntity monitor (clk, rst, en,
                       data, 
                       mem0, mem1, mem2, mem3, mem4 
                    );

    always begin
        #1 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #1;
        data = 1;
        #1;
        data = 2;
        #1;
        data = 3;
        #1;
        data = 4;
        #1
        data = 5;
        #1;
        $finish;
    end

endmodule

