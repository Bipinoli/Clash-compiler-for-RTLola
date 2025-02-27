`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] input_x;
    reg new_input;

    wire signed [63:0] a;
    wire aktv_a;
    wire signed [63:0] b;
    wire aktv_b;

    wire signed [63:0] hlc_timer;
    wire pacing_a;
    wire pacing_b;
    wire slide_b;

    wire signed [2:0] llc_state;
    wire signed [63:0] sw_b_0;
    wire signed [63:0] sw_b_1;
    wire signed [63:0] sw_b_2;
    wire signed [63:0] sw_b_3;

    wire push_valid;
    wire pop_valid;
    wire qPush;
    wire qPop;

    wire signed [63:0] llc_x;

    topEntity monitor (clk, rst, en,
                       input_x, new_input,
                       a, aktv_a, b, aktv_b,
                       hlc_timer, llc_state,
                       sw_b_0, sw_b_1, sw_b_2, sw_b_3,
                       qPush, qPop, push_valid, pop_valid,
                       llc_x, pacing_a, pacing_b, slide_b
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

