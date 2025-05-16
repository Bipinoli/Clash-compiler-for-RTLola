`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    
    reg signed [63:0] input_0;
    reg new_input_0;

    wire signed [63:0] output_0;
    wire output_0_aktv;
    wire signed [63:0] output_1;
    wire output_1_aktv;
    wire signed [63:0] output_2;
    wire output_2_aktv;
    wire signed [63:0] output_3;
    wire output_3_aktv;
    wire signed [63:0] output_4;
    wire output_4_aktv;
    wire signed [63:0] output_5;
    wire output_5_aktv;
    wire signed [63:0] output_6;
    wire output_6_aktv;
    wire signed [63:0] output_7;
    wire output_7_aktv;
    wire signed [63:0] output_8;
    wire output_8_aktv;
    wire signed [63:0] output_9;
    wire output_9_aktv;
    wire signed [63:0] output_10;
    wire output_10_aktv;
    wire signed [63:0] output_11;
    wire output_11_aktv;
    wire signed [63:0] output_12;
    wire output_12_aktv;
    wire signed [63:0] output_13;
    wire output_13_aktv;

    wire q_push;
    wire q_pop;
    wire q_push_valid;
    wire q_pop_valid;

    wire pacing_0;
    wire pacing_1;
    wire pacing_2;
    wire pacing_3;
    wire pacing_4;
    wire pacing_5;
    wire pacing_6;
    wire pacing_7;
    wire pacing_8;
    wire pacing_9;
    wire pacing_10;
    wire pacing_11;
    wire pacing_12;
    wire pacing_13;


    wire signed [7:0] h_t;
    wire signed [7:0] h_tag;
    wire signed [63:0] h;
    wire signed [7:0] g_tag;
    wire signed [63:0] g;
    wire signed [7:0] n_tag;
    wire signed [63:0] n;
    

    topEntity monitor (
                        clk, rst, en
                        ,input_0, new_input_0
                        ,output_0, output_0_aktv
                        ,output_1, output_1_aktv
                        ,output_2, output_2_aktv
                        ,output_3, output_3_aktv
                        ,output_4, output_4_aktv
                        ,output_5, output_5_aktv
                        ,output_6, output_6_aktv
                        ,output_7, output_7_aktv
                        ,output_8, output_8_aktv
                        ,output_9, output_9_aktv
                        ,output_10, output_10_aktv
                        ,output_11, output_11_aktv
                        ,output_12, output_12_aktv
                        ,output_13, output_13_aktv
                        ,q_push, q_pop, q_push_valid, q_pop_valid
                        ,pacing_0
                        ,pacing_1
                        ,pacing_2
                        ,pacing_3
                        ,pacing_4
                        ,pacing_5
                        ,pacing_6
                        ,pacing_7
                        ,pacing_8
                        ,pacing_9
                        ,pacing_10
                        ,pacing_11
                        ,pacing_12
                        ,pacing_13
                        ,h_tag, h, g_tag, g, n_tag, n, h_t
                       );

    always begin
        #1 clk = ~clk;
    end

    always @(posedge clk) begin
        if (output_0_aktv == 1'b1 || output_1_aktv == 1'b1 || output_2_aktv == 1'b1 || output_3_aktv == 1'b1 || output_4_aktv == 1'b1 || output_5_aktv == 1'b1 || output_6_aktv == 1'b1 || output_7_aktv == 1'b1 || output_8_aktv == 1'b1 || output_9_aktv == 1'b1 || output_10_aktv == 1'b1 || output_11_aktv == 1'b1 || output_12_aktv == 1'b1 || output_13_aktv == 1'b1) begin
            $display("Time %0t: Active outputs: (%b, %b, %b, %b, %b, %b, %b, %b, %b, %b, %b, %b, %b, %b), Outputs: (%0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d)", $time, output_0_aktv, output_1_aktv, output_2_aktv, output_3_aktv, output_4_aktv, output_5_aktv, output_6_aktv, output_7_aktv, output_8_aktv, output_9_aktv, output_10_aktv, output_11_aktv, output_12_aktv, output_13_aktv, output_0, output_1, output_2, output_3, output_4, output_5, output_6, output_7, output_8, output_9, output_10, output_11, output_12, output_13);
        end
    end

    initial begin
        #0; // important to make sure that assignments happens before evaluation to avoid undefined values in the beginning
        clk = 1;
        rst = 1;
        en = 1;
        new_input_0 = 0;

        #2;
        rst = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 1;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #298;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 2;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #98;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 3;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #398;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 4;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #1298;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 5;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #898;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 6;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #1098;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 7;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #3098;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 8;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;


        #40;
        $finish; 
    end

endmodule

