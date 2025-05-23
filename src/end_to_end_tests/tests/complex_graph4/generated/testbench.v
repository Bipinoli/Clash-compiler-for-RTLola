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

    wire q_push;
    wire q_pop;
    wire q_push_valid;
    wire q_pop_valid;

    wire pacing_in0;
    wire pacing_in1;
    wire pacing_out0_0;
    wire pacing_out1_0;
    wire pacing_out2_0;
    wire pacing_out3_0;
    wire pacing_out4_0;
    wire pacing_out5_0;
    wire pacing_out5_1;
    wire pacing_out6_0;
    wire pacing_out6_1;
    wire pacing_out7_0;
    wire pacing_out7_1;
    wire pacing_out8_0;
    wire pacing_out9_0;
    wire pacing_out10_0;
    wire pacing_out11_0;
    wire slide_0;
    wire slide_1;
    

    topEntity monitor (
                        clk, rst, en
                        ,input_0, new_input_0
                        ,input_1, new_input_1
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
                        ,q_push, q_pop, q_push_valid, q_pop_valid
                        ,pacing_in0
                        ,pacing_in1
                        ,pacing_out0_0
                        ,pacing_out1_0
                        ,pacing_out2_0
                        ,pacing_out3_0
                        ,pacing_out4_0
                        ,pacing_out5_0
                        ,pacing_out5_1
                        ,pacing_out6_0
                        ,pacing_out6_1
                        ,pacing_out7_0
                        ,pacing_out7_1
                        ,pacing_out8_0
                        ,pacing_out9_0
                        ,pacing_out10_0
                        ,pacing_out11_0
                        ,slide_0
                        ,slide_1
                       );

    always begin
        #1 clk = ~clk;
    end

    always @(posedge clk) begin
        if (output_0_aktv == 1'b1 || output_1_aktv == 1'b1 || output_2_aktv == 1'b1 || output_3_aktv == 1'b1 || output_4_aktv == 1'b1 || output_5_aktv == 1'b1 || output_6_aktv == 1'b1 || output_7_aktv == 1'b1 || output_8_aktv == 1'b1 || output_9_aktv == 1'b1 || output_10_aktv == 1'b1 || output_11_aktv == 1'b1) begin
            $display("Time %0t: Active outputs: (%b, %b, %b, %b, %b, %b, %b, %b, %b, %b, %b, %b), Outputs: (%0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d)", $time, output_0_aktv, output_1_aktv, output_2_aktv, output_3_aktv, output_4_aktv, output_5_aktv, output_6_aktv, output_7_aktv, output_8_aktv, output_9_aktv, output_10_aktv, output_11_aktv, output_0, output_1, output_2, output_3, output_4, output_5, output_6, output_7, output_8, output_9, output_10, output_11);
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

        #6;
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

        #2.0000000000001137;
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

        #0;
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

        #1983;
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

        #996;
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

        #4998;
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

        #10998;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 1;
        input_1 = 4;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #8998;
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

        #4998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 6;
        new_input_1 = 0;
        input_1 = 0;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #4998;
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

        #2498;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 8;
        new_input_1 = 0;
        input_1 = 0;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #12498;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 9;
        new_input_1 = 1;
        input_1 = 9;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #12498;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 10;
        new_input_1 = 1;
        input_1 = 10;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;

        #2498;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 11;
        new_input_1 = 1;
        input_1 = 11;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;


        #40;
        $finish; 
    end

endmodule

