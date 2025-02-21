`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] input_x;
    reg new_input;

    wire signed [63:0] event_input_x;
    wire event_input_new;
    wire slideBC;
    wire slideD;
    wire pacingBC;
    wire pacingD;
    wire signed [63:0] timer1;

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
    wire signed [63:0] winX_2;
    wire slideB;

    wire signed [63:0] outA;
    wire aktvOutA;
    wire signed [63:0] outB;
    wire aktvOutB;

    topEntity monitor (clk, rst, en,
                       input_x, new_input,
                       event_input_x, event_input_new,
                       slideBC, slideD,
                       pacingBC, pacingD,
                       timer1
                       );

    always begin
        #1 clk = ~clk;
    end

    initial begin
        clk = 1;
        rst = 0;
        en = 0;
        new_input = 0;
        input_x = 0;

        #1;
        en = 1;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #999; // 0.0010s
        input_x = 1;
        new_input = 1;
        #2;
        input_x = 0;
        new_input = 0;

        #498; // 0.0005s
        input_x = 2;
        new_input = 1;
        #2;
        input_x = 0;
        new_input = 0;

        #498;
        input_x = 3;
        new_input = 1;
        #2;
        input_x = 0;
        new_input = 0;

        #498;
        input_x = 4;
        new_input = 1;
        #2;
        input_x = 0;
        new_input = 0;

        #498;
        input_x = 5;
        new_input = 1;
        #2;
        input_x = 0;
        new_input = 0;

        #498;
        input_x = 6;
        new_input = 1;
        #2;
        input_x = 0;
        new_input = 0;

        #498;
        input_x = 7;
        new_input = 1;
        #2;
        input_x = 0;
        new_input = 0;

        #498;
        input_x = 8;
        new_input = 1;
        #2;
        input_x = 0;
        new_input = 0;

        #498;
        input_x = 9;
        new_input = 1;
        #2;
        input_x = 0;
        new_input = 0;

        #60;
        $finish; 
    end

endmodule

