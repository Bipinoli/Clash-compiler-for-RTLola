`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    
    reg signed [63:0] input_0;
    reg new_input_0;
    reg signed [63:0] input_1;
    reg new_input_1;
    reg signed [63:0] input_2;
    reg new_input_2;

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
    wire slide_1;
    

    topEntity monitor (
                        clk, rst, en
                        ,input_0, new_input_0
                        ,input_1, new_input_1
                        ,input_2, new_input_2
                        ,output_0, output_0_aktv
                        ,output_1, output_1_aktv
                        ,output_2, output_2_aktv
                        ,q_push, q_pop, q_push_valid, q_pop_valid
                        ,pacing_0
                        ,pacing_1
                        ,pacing_2
                        ,slide_0
                        ,slide_1
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
        new_input_2 = 0;

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
        new_input_2 = 1;
        input_2 = 1;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #6;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 2;
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

        #2.0000000000001137;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 3;
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

        #1.9999999999998863;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 4;
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

        #1983;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 5;
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

        #1008;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
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

        #0;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 7;
        new_input_1 = 1;
        input_1 = 7;
        new_input_2 = 1;
        input_2 = 7;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #0.9999999999995453;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 8;
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

        #0.0000000000004547473508864641;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 9;
        new_input_1 = 1;
        input_1 = 9;
        new_input_2 = 1;
        input_2 = 9;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #0;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 10;
        new_input_1 = 1;
        input_1 = 10;
        new_input_2 = 1;
        input_2 = 10;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #0.9999999999995453;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 11;
        new_input_1 = 1;
        input_1 = 11;
        new_input_2 = 0;
        input_2 = 0;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #1075.0000000000005;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 12;
        new_input_1 = 1;
        input_1 = 12;
        new_input_2 = 0;
        input_2 = 0;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;
        new_input_1 = 0;
        input_1 = 0;
        new_input_2 = 0;
        input_2 = 0;

        #1;
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

        #3095;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 14;
        new_input_1 = 1;
        input_1 = 14;
        new_input_2 = 1;
        input_2 = 14;
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

