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

    

    topEntity monitor (
                        clk, rst, en
                        ,input_0, new_input_0
                        ,output_0, output_0_aktv
                        ,output_1, output_1_aktv
                        ,output_2, output_2_aktv
                        ,output_3, output_3_aktv
                       );

    always begin
        #1 clk = ~clk;
    end

    always @(posedge clk) begin
        if (output_0_aktv == 1'b1 || output_1_aktv == 1'b1 || output_2_aktv == 1'b1 || output_3_aktv == 1'b1) begin
            $display("Time %0t: Active outputs: (%b, %b, %b, %b), Outputs: (%0d, %0d, %0d, %0d)", $time, output_0_aktv, output_1_aktv, output_2_aktv, output_3_aktv, output_0, output_1, output_2, output_3);
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

        #6;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 2;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #2.0000000000001137;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 3;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #1.9999999999998863;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 4;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #1983;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 5;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #1008;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 6;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #0;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 7;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #0.9999999999995453;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 8;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #0.0000000000004547473508864641;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 9;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #0;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 10;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #0.9999999999995453;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 11;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #1075.0000000000005;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 12;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #1;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 13;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;

        #3095;
        @(posedge clk);
        new_input_0 = 1;
        input_0 = 14;
        @(posedge clk);
        new_input_0 = 0;
        input_0 = 0;


        #40;
        $finish; 
    end

endmodule

