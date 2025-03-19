`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] input_x;
    reg new_input;

    wire signed [63:0] output_a;
    wire output_a_aktv;
    wire signed [63:0] output_b;
    wire output_b_aktv;
    wire signed [63:0] output_c;
    wire output_c_aktv;

    wire [2:0] llc_state;
    wire qPush;
    wire qPop;
    wire qPushValid;
    wire qPopValid;
    wire signed [63:0] llc_x;
    wire pacingABC;


    topEntity monitor (clk, rst, en,
                       input_x, new_input,
                       output_a, output_a_aktv,
                       output_b, output_b_aktv,
                       output_c, output_c_aktv,
                       llc_state, qPush, qPop, qPushValid, qPopValid, llc_x, pacingABC
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
        #2; new_input = 0;
        
        #10;
        input_x = 2;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_x = 3;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_x = 4;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_x = 5;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_x = 6;
        new_input = 1;
        #2; new_input = 0;

        #12;
        $finish; 
    end

endmodule

