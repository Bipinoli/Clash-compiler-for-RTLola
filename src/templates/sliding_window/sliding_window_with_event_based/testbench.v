`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] input_x;
    reg new_input;

    wire qPush;
    wire qPop;
    wire signed [63:0] qInX;
    wire qPushValid;
    wire qPopValid;
    wire signed [63:0] qOutX;
    wire qOutNewX;
    wire signed [63:0] qWaitX;
    wire enA;
    wire enB;
    wire signed [63:0] stage;
    wire signed [63:0] timerB;
    wire signed [63:0] winX_0;
    wire signed [63:0] winX_1;

    wire signed [63:0] outA;
    wire aktvOutA;
    wire signed [63:0] outB;
    wire aktvOutB;

    topEntity monitor (clk, rst, en,
                       input_x, new_input,
                       qPush, qPop, qInX, qPushValid, qPopValid, qOutX, qOutNewX, qWaitX, enA, enB, stage, timerB, winX_0, winX_1,
                       outA, aktvOutA, outB, aktvOutB
                       );

    always begin
        #1 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 0;
        new_input = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        // 5 cycles = 5 * #2;
        #10;
        input_x = 1;
        new_input = 1;
        en = 1;

        #2; // 0.001 seconds
        input_x = 2;
        new_input = 1;

        #2; // 0.001 seconds
        input_x = 3;
        new_input = 1;
        
        #2; // 0.001 seconds
        input_x = 4;
        new_input = 1;

        #2; 
        new_input = 0;
        
        #10;
        input_x = 5;
        new_input = 1;

        #2; 
        new_input = 0;
        
        #40;
        $finish; 
    end

endmodule

