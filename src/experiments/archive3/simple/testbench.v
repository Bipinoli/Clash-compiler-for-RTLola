`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    
    reg signed [63:0] input_0;
    reg new_input_0;
    reg signed [63:0] input_1;
    reg new_input_1;

    wire signed [63:0] output_0;
    wire output_0_aktv;
    wire signed [63:0] output_1;
    wire output_1_aktv;
    wire signed [63:0] output_2;
    wire output_2_aktv;

    wire signed [63:0] llc_tag;
    wire llc_to_pop;
    wire llc_is_valid_event;
    wire q_push_valid;
    wire q_pop_valid;

    topEntity monitor (
                        clk, rst, en,
                        input_0, new_input_0,
                        input_1, new_input_1,
                        output_0, output_0_aktv,
                        output_1, output_1_aktv,
                        output_2, output_2_aktv
                        ,llc_tag, llc_to_pop, llc_is_valid_event, q_push_valid, q_pop_valid 
                       );

    always begin
        #1 clk = ~clk;
    end

    initial begin
        clk = 1;
        rst = 0;
        en = 1;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #1000;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 1;
        new_input_1 = 1;
        input_1 = 1;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 2;
        new_input_1 = 1;
        input_1 = 2;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 3;
        new_input_1 = 1;
        input_1 = 3;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 4;
        new_input_1 = 1;
        input_1 = 4;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;


        #40;
        $finish; 
    end

endmodule

