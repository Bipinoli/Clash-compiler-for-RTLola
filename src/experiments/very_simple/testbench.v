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

    wire q_push;
    wire q_pop;
    wire q_push_valid;
    wire q_pop_valid;

    wire pacing_0;
    wire pacing_1;
    wire pacing_2;
    wire slide_0;

    wire signed [7:0] sw_t;
    wire signed [63:0] sw_v1;
    wire signed [63:0] sw_v2;
    wire signed [63:0] sw_v3;
    
    wire signed [7:0] out_0_t1;

    wire signed [7:0] tag_in_0;
    wire signed [63:0] out_0_v1;
    wire signed [7:0] out_0_t2;
    wire signed [63:0] out_0_v2;
    wire signed [7:0] out_0_t3;
    wire signed [63:0] out_0_v3;

    

    topEntity monitor (
                        clk, rst, en
                        ,input_0, new_input_0
                        ,input_1, new_input_1
                        ,output_0, output_0_aktv
                        ,output_1, output_1_aktv
                        ,output_2, output_2_aktv
                        ,q_push, q_pop, q_push_valid, q_pop_valid
                        ,pacing_0
                        ,pacing_1
                        ,pacing_2
                        ,slide_0
                        ,tag_in_0
                        ,sw_t, sw_v1, sw_v2, sw_v3
                        ,out_0_t1
                        ,out_0_v1
                        ,out_0_t2
                        ,out_0_v2
                        ,out_0_t3
                        ,out_0_v3
                       );

    always begin
        #1 clk = ~clk;
    end

    always @(posedge clk) begin
        if (output_0_aktv == 1'b1 || output_1_aktv == 1'b1 || output_2_aktv == 1'b1) begin
            $display("Time %0t: Active outputs: (%b, %b, %b), Outputs: (%0d, %0d, %0d)", $time, output_0_aktv, output_1_aktv, output_2_aktv, output_0, output_1, output_2);
        end
    end

    initial begin
        #0; // important to make sure that assignments happens before evaluation to avoid undefined values in the beginning
        clk = 1;
        rst = 1;
        en = 1;
        new_input_0 = 0;
        new_input_1 = 0;

        #2;
        rst = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #998;
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

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 5;
        new_input_1 = 1;
        input_1 = 5;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #1998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 7;
        new_input_1 = 1;
        input_1 = 7;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 8;
        new_input_1 = 1;
        input_1 = 8;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;


        #40;
        $finish; 
    end

endmodule

