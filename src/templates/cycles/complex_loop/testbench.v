`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [7:0] input_a;
    reg new_input_a;
    reg signed [7:0] input_b;
    reg new_input_b;

    wire signed [7:0] output_c;
    wire output_c_aktv;
    wire signed [7:0] output_d;
    wire output_d_aktv;
    wire signed [7:0] output_e;
    wire output_e_aktv;
    wire signed [7:0] output_f;
    wire output_f_aktv;
    wire signed [7:0] output_g;
    wire output_g_aktv;
    wire signed [7:0] output_ts;
    wire output_ts_aktv;

    wire qPush;
    wire qPop;
    wire qPushValid;
    wire qPopValid;
    wire signed [2:0] llc_state;
    wire signed [7:0] llc_a;
    wire signed [7:0] llc_b;

    wire periodic_pacing;

    topEntity monitor (clk, rst, en,
                       input_a, new_input_a,
                       input_b, new_input_b,
                       output_c, output_c_aktv,
                       output_d, output_d_aktv,
                       output_e, output_e_aktv,
                       output_f, output_f_aktv,
                       output_g, output_g_aktv,
                       output_ts, output_ts_aktv,
                       qPush, qPop, qPushValid, qPopValid, 
                       llc_state, llc_a, llc_b, periodic_pacing
                       );

    always begin
        #1 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst = 0;
        en = 1;
        input_a = 0;
        input_b = 0;
        new_input_a = 0;
        new_input_b = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #10;
        input_a = 1;
        input_b = 1;
        new_input_a = 1;
        new_input_b = 1;
        #2; 
        new_input_a = 0;
        new_input_b = 0;
        
        #1000;
        input_b = 2;
        new_input_a = 0;
        new_input_b = 1;
        #2; 
        new_input_a = 0;
        new_input_b = 0;

        #1000;
        input_a = 3;
        input_b = 3;
        new_input_a = 1;
        new_input_b = 1;
        #2; 
        new_input_a = 0;
        new_input_b = 0;

        #2000;
        input_a = 4;
        input_b = 4;
        new_input_a = 1;
        new_input_b = 1;
        #2; 
        new_input_a = 0;
        new_input_b = 0;

        #1000;
        input_a = 5;
        new_input_a = 1;
        new_input_b = 0;
        #2; 
        new_input_a = 0;
        new_input_b = 0;

        #1000;
        input_a = 6;
        input_b = 6;
        new_input_a = 1;
        new_input_b = 1;
        #2; 
        new_input_a = 0;
        new_input_b = 0;

        #1000;
        input_a = 7;
        input_b = 7;
        new_input_a = 1;
        new_input_b = 1;
        #2; 
        new_input_a = 0;
        new_input_b = 0;

        #12;
        $finish; 
    end

endmodule

