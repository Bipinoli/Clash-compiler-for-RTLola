`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg push;
    reg pop;
    reg signed [63:0] data;

    wire push_valid;
    wire pop_valid;
    wire signed [63:0] out;
    wire signed [63:0] mem0;
    wire signed [63:0] mem1;
    wire signed [63:0] mem2;
    wire signed [63:0] mem3;
    wire signed [63:0] mem4;
    wire signed [63:0] cursor;

    topEntity monitor (clk, rst, en,
                       push, pop, data,
                       push_valid, pop_valid, out,
                       mem0, mem1, mem2, mem3, mem4, cursor
                    );

    always begin
        #1 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        push = 0;
        pop = 0;
        data = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #10;
        push = 1;
        pop = 0;
        data = 1;
        // 1 _ _ _ _ 
        // (push_valid, pop_valid, output) = (1, 0, 0)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 1;
        pop = 0;
        data = 2;
        // 2 1 _ _ _ 
        // (push_valid, pop_valid, output) = (1, 0, 0)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 1;
        pop = 0;
        data = 3;
        // 3 2 1 _ _ 
        // (push_valid, pop_valid, output) = (1, 0, 0)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 0;
        pop = 1;
        data = 0;
        // 3 2 _ _ _ 
        // (push_valid, pop_valid, output) = (0, 1, 1)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 1;
        pop = 1;
        data = 4;
        // 4 3 _ _ _ 
        // (push_valid, pop_valid, output) = (1, 1, 2)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 1;
        pop = 1;
        data = 5;
        // 5 4 _ _ _ 
        // (push_valid, pop_valid, output) = (1, 1, 3)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 0;
        pop = 1;
        data = 0;
        // 5 _ _ _ _ 
        // (push_valid, pop_valid, output) = (0, 1, 4)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 1;
        pop = 1;
        data = 6;
        // 6 _ _ _ _ 
        // (push_valid, pop_valid, output) = (1, 1, 5)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 0;
        pop = 1;
        data = 0;
        // _ _ _ _ _ 
        // (push_valid, pop_valid, output) = (0, 1, 6)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 1;
        pop = 1;
        data = 7;
        // 7 _ _ _ _ 
        // (push_valid, pop_valid, output) = (1, 0, 0)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 0;
        pop = 1;
        data = 0;
        // _ _ _ _ _ 
        // (push_valid, pop_valid, output) = (0, 1, 7)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 0;
        pop = 1;
        data = 0;
        // _ _ _ _ _ 
        // (push_valid, pop_valid, output) = (0, 0, 0)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        push = 0;
        pop = 1;
        data = 0;
        // _ _ _ _ _ 
        // (push_valid, pop_valid, output) = (0, 0, 0)
        #1;
        push = 0;
        pop = 0;
        data = 0;

        #10;
        $finish; 
    end

endmodule

