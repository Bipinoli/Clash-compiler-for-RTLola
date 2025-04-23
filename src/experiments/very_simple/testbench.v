`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    
    reg signed [63:0] input_0;
    reg new_input_0;

    wire signed [63:0] output_0;
    wire output_0_aktv;

    wire signed [63:0] llc_tag;
    wire llc_to_pop;
    wire llc_is_valid_event;
    wire q_push_valid;
    wire q_pop_valid;
    wire new_event;
    wire q_push;
    wire q_pop;

    topEntity monitor (
                        clk, rst, en,
                        input_0, new_input_0,
                        output_0, output_0_aktv
                        ,llc_tag, llc_to_pop, llc_is_valid_event, q_push_valid, q_pop_valid, new_event, q_push, q_pop
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
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 2;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 3;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 4;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;


        #40;
        $finish; 
    end

endmodule

