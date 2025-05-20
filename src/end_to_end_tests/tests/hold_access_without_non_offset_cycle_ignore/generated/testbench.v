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

    wire q_push;
    wire q_pop;
    wire q_push_valid;
    wire q_pop_valid;

    wire pacing_0;
    wire pacing_1;
    

    topEntity monitor (
                        clk, rst, en
                        ,input_0, new_input_0
                        ,output_0, output_0_aktv
                        ,output_1, output_1_aktv
                        ,q_push, q_pop, q_push_valid, q_pop_valid
                        ,pacing_0
                        ,pacing_1
                       );

    always begin
        #1 clk = ~clk;
    end

    always @(posedge clk) begin
        if (output_0_aktv == 1'b1 || output_1_aktv == 1'b1) begin
            $display("Time %0t: Active outputs: (%b, %b), Outputs: (%0d, %0d)", $time, output_0_aktv, output_1_aktv, output_0, output_1);
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

