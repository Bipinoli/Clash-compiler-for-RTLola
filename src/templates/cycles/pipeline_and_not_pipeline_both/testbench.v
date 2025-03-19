`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] input_x;
    reg new_input;

    wire signed [63:0] output_d;
    wire output_d_aktv;
    wire signed [63:0] output_e;
    wire output_e_aktv;
    wire signed [63:0] output_f;
    wire output_f_aktv;

    wire signed [63:0] winF_0;
    wire signed [63:0] winF_1;

    wire [2:0] llc_state;
    wire qPush;
    wire qPop;
    wire qPushValid;
    wire qPopValid;
    wire signed [63:0] llc_x;
    wire pacingABC;


    topEntity monitor (clk, rst, en,
                       input_x, new_input,
                       output_d, output_d_aktv,
                       output_e, output_e_aktv,
                       output_f, output_f_aktv,
                       winF_0, winF_1
                       );

    always begin
        #1 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        new_input = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #10;
        input_x = 1;
        new_input = 1;
        
        #2;
        input_x = 2;
        new_input = 1;

        #2;
        input_x = 3;
        new_input = 1;

        #2;
        input_x = 4;
        new_input = 1;

        #2;
        input_x = 5;
        new_input = 1;
        #2; new_input = 0;

        #4;
        input_x = 6;
        new_input = 1;
        #2; new_input = 0;

        #2;
        input_x = 7;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_x = 8;
        new_input = 1;
        #2; new_input = 0;

        #12;
        $finish; 
    end

endmodule

