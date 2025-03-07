`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [7:0] input_a;
    reg new_input;

    wire signed [7:0] output_b;
    wire output_b_aktv;
    wire signed [7:0] output_c;
    wire output_c_aktv;

    topEntity monitor (clk, rst, en,
                       input_a, new_input,
                       output_b, output_b_aktv,
                       output_c, output_c_aktv
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
        input_a = 1;
        new_input = 1;
        #2; new_input = 0;
        
        #10;
        input_a = 2;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_a = 3;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_a = 4;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_a = 5;
        new_input = 1;
        #2; new_input = 0;

        #10;
        input_a = 6;
        new_input = 1;
        #2; new_input = 0;

        #12;
        $finish; 
    end

endmodule

