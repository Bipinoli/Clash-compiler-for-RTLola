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


    wire signed [63:0] output_d;
    wire output_d_aktv;
    wire signed [63:0] output_e;
    wire output_e_aktv;
    wire signed [63:0] output_f;
    wire output_f_aktv;

    wire signed [7:0] tag_0;
    wire signed [63:0] winF_0;
    wire signed [7:0] tag_1;
    wire signed [63:0] winF_1;
    wire signed [7:0] tag_2;
    wire signed [63:0] winF_2;
    wire signed [7:0] tag_3;
    wire signed [63:0] winF_3;
    wire signed [7:0] tag_4;
    wire signed [63:0] winF_4;


    wire [2:0] llc_state;
    wire signed [63:0] llc_x;
    wire pacing_abc;
    wire q_push_valid;
    wire q_pop_valid;


    topEntity monitor (clk, rst, en,
                       input_x, new_input,
                       output_a, output_a_aktv,
                       output_b, output_b_aktv,
                       output_c, output_c_aktv,
                       output_d, output_d_aktv,
                       output_e, output_e_aktv,
                       output_f, output_f_aktv,
                       llc_state, llc_x, pacing_abc,
                       q_push_valid, q_pop_valid
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

        #4;
        input_x = 9;
        new_input = 1;

        #2;
        input_x = 10;
        new_input = 1; 

        #2;
        input_x = 11;
        new_input = 1; 

        #2;
        input_x = 12;
        new_input = 1; 

        #2;
        input_x = 13;
        new_input = 1; 

        #2;
        input_x = 14;
        new_input = 1; 

        #2;
        input_x = 15;
        new_input = 1; 

        #2;
        input_x = 16;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_x = 17;
        new_input = 1; 

        #2;
        input_x = 18;
        new_input = 1; 
        #2; new_input = 0;

        #40;
        $finish; 
    end

endmodule

