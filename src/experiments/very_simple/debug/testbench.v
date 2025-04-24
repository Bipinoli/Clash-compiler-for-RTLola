`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg push; 
    reg pop;
    wire signed [63:0] cursor;

    topEntity monitor (
                        clk, rst, en,
                        push, pop, cursor
                       );

    always begin
        #1 clk = ~clk;
    end

    initial begin
        clk = 1;
        rst = 1;
        en = 1;
        push = 0;
        pop = 0;

        #1;
        rst = 0;

        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #10;
        @(posedge clk);
        push = 1;
        pop = 0;
        @(posedge clk);
        push = 0; 
        pop = 0;

        #10;
        @(posedge clk);
        push = 1;
        pop = 1;
        @(posedge clk);
        push = 0; 
        pop = 0;

        #10;
        @(posedge clk);
        push = 0;
        pop = 0;
        @(posedge clk);
        push = 0; 
        pop = 0;

        #10;
        @(posedge clk);
        push = 0;
        pop = 1;
        @(posedge clk);
        push = 0; 
        pop = 0;

        #40;
        $finish; 
    end

endmodule

