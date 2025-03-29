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

    topEntity monitor (clk, rst, en,
                       input_x, new_input,
                       output_a, output_a_aktv,
                       output_b, output_b_aktv,
                       output_c, output_c_aktv,
                       output_d, output_d_aktv
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

