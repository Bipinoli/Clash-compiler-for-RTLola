`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    
    reg signed [63:0] input_0;
    reg new_input_0;
    reg unsigned [63:0] input_1;
    reg new_input_1;
    reg signed [63:0] input_2;
    reg new_input_2;

    wire signed [63:0] output_0;
    wire output_0_aktv;
    wire  output_1;
    wire output_1_aktv;
    wire  output_2;
    wire output_2_aktv;
    wire  output_3;
    wire output_3_aktv;
    wire unsigned [63:0] output_4;
    wire output_4_aktv;
    wire  output_5;
    wire output_5_aktv;
    wire  output_6;
    wire output_6_aktv;
    wire  output_7;
    wire output_7_aktv;
    wire  output_8;
    wire output_8_aktv;
    wire  output_9;
    wire output_9_aktv;
    wire  output_10;
    wire output_10_aktv;

    wire q_push;
    wire q_pop;
    wire q_push_valid;
    wire q_pop_valid;

    wire slide_0;
    wire slide_1;
    wire slide_2;
    wire slide_3;
    wire slide_4;
    wire slide_5;
    wire slide_6;
    wire enable_in0;
    wire enable_in1;
    wire enable_in2;
    wire enable_out0;
    wire enable_out1;
    wire enable_out2;
    wire enable_out3;
    wire enable_out4;
    wire enable_out5;
    wire enable_out6;
    wire enable_out7;
    wire enable_out8;
    wire enable_out9;
    wire enable_out10;
    

    topEntity monitor (
                        clk, rst, en
                        ,input_0, new_input_0
                        ,input_1, new_input_1
                        ,input_2, new_input_2
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
                        ,q_push, q_pop, q_push_valid, q_pop_valid
                        ,slide_0
                        ,slide_1
                        ,slide_2
                        ,slide_3
                        ,slide_4
                        ,slide_5
                        ,slide_6
                        ,enable_in0
                        ,enable_in1
                        ,enable_in2
                        ,enable_out0
                        ,enable_out1
                        ,enable_out2
                        ,enable_out3
                        ,enable_out4
                        ,enable_out5
                        ,enable_out6
                        ,enable_out7
                        ,enable_out8
                        ,enable_out9
                        ,enable_out10
                       );

    always begin
        #1 clk = ~clk;
    end

    always @(posedge clk) begin
        if (output_0_aktv == 1'b1 || output_1_aktv == 1'b1 || output_2_aktv == 1'b1 || output_3_aktv == 1'b1 || output_4_aktv == 1'b1 || output_5_aktv == 1'b1 || output_6_aktv == 1'b1 || output_7_aktv == 1'b1 || output_8_aktv == 1'b1 || output_9_aktv == 1'b1 || output_10_aktv == 1'b1) begin
            $display("Time %0t: Active outputs: (%b, %b, %b, %b, %b, %b, %b, %b, %b, %b, %b), Outputs: (%0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d, %0d)", $time, output_0_aktv, output_1_aktv, output_2_aktv, output_3_aktv, output_4_aktv, output_5_aktv, output_6_aktv, output_7_aktv, output_8_aktv, output_9_aktv, output_10_aktv, output_0, output_1, output_2, output_3, output_4, output_5, output_6, output_7, output_8, output_9, output_10);
        end
    end

    initial begin
        #0; // important to make sure that assignments happens before evaluation to avoid undefined values in the beginning
        clk = 1;
        rst = 1;
        en = 1;
        new_input_0 = 0;
        new_input_1 = 0;
        new_input_2 = 0;

        #2;
        rst = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #98;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 1;
        new_input_1 = 1;
        input_1 = 1;
        new_input_2 = 1;
        input_2 = 1;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #698;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 10;
        new_input_1 = 1;
        input_1 = 2;
        new_input_2 = 1;
        input_2 = 2;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #98;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = -20;
        new_input_1 = 1;
        input_1 = 3;
        new_input_2 = 1;
        input_2 = 3;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #798;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 30;
        new_input_1 = 1;
        input_1 = 4;
        new_input_2 = 1;
        input_2 = 4;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #898;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = -40;
        new_input_1 = 1;
        input_1 = 5;
        new_input_2 = 1;
        input_2 = 5;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #398;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 50;
        new_input_1 = 1;
        input_1 = 6;
        new_input_2 = 1;
        input_2 = 6;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #498;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = -60;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #498;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 1;
        input_1 = 7;
        new_input_2 = 0;
        input_2 = 0;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #498;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 1;
        input_2 = 8;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #298;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = -19;
        new_input_1 = 1;
        input_1 = 9;
        new_input_2 = 0;
        input_2 = 0;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #198;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 10;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 1;
        input_2 = 10;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #798;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 1;
        input_1 = 11;
        new_input_2 = 1;
        input_2 = 11;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #898;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = -12;
        new_input_1 = 1;
        input_1 = 12;
        new_input_2 = 1;
        input_2 = 12;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #998;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 13;
        new_input_1 = 1;
        input_1 = 13;
        new_input_2 = 1;
        input_2 = 13;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #498;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = -26;
        new_input_1 = 1;
        input_1 = 16;
        new_input_2 = 1;
        input_2 = 16;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;


        #40;
        $finish; 
    end

endmodule

