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
    wire signed [63:0] waited;

    topEntity monitor (clk, rst, en,
                       push, pop, data,
                       push_valid, pop_valid, out, waited
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
        #1; 
        push = 0;
        pop = 0;

        #1;
        push = 1;
        pop = 0;
        data = 2;
        #1;
        push = 0;
        pop = 0;

        #1;
        push = 1;
        pop = 0;
        data = 3;
        #1;
        push = 0;
        pop = 0;

        #1;
        push = 1;
        pop = 0;
        data = 4;
        #1;
        push = 0;
        pop = 0;

        #1;
        push = 0;
        pop = 1;
        data = 0;
        #1;
        push = 0;
        pop = 0;

        // Note: 
        // #4 is not enough because the data is not available when the clock raises to high
        // It is important that the input is sustained for a single clock cycle for things to work correctly
        #4;
        push = 1;
        pop = 0;
        data = 5;
        #1;
        push = 0;
        pop = 0;

        #1;
        push = 0;
        pop = 1;
        data = 0;
        #1;
        push = 0;
        pop = 0;


        #10;
        $finish; 
    end

endmodule

