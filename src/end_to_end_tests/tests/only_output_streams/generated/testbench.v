`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    

    wire signed [63:0] output_0;
    wire output_0_aktv;
    wire signed [63:0] output_1;
    wire output_1_aktv;

    wire q_push;
    wire q_pop;
    wire q_push_valid;
    wire q_pop_valid;

    wire enable_out0;
    wire enable_out1;
    

    topEntity monitor (
                        clk, rst, en
                        ,output_0, output_0_aktv
                        ,output_1, output_1_aktv
                        ,q_push, q_pop, q_push_valid, q_pop_valid
                        ,enable_out0
                        ,enable_out1
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

        #2;
        rst = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        #998;
        @(posedge clk);
        @(posedge clk);

        #6;
        @(posedge clk);
        @(posedge clk);

        #2.0000000000001137;
        @(posedge clk);
        @(posedge clk);

        #0;
        @(posedge clk);
        @(posedge clk);

        #1983;
        @(posedge clk);
        @(posedge clk);

        #1007;
        @(posedge clk);
        @(posedge clk);

        #0;
        @(posedge clk);
        @(posedge clk);

        #0;
        @(posedge clk);
        @(posedge clk);

        #0;
        @(posedge clk);
        @(posedge clk);

        #0;
        @(posedge clk);
        @(posedge clk);

        #0.9999999999995453;
        @(posedge clk);
        @(posedge clk);

        #1074;
        @(posedge clk);
        @(posedge clk);

        #1;
        @(posedge clk);
        @(posedge clk);

        #3094;
        @(posedge clk);
        @(posedge clk);


        #40;
        $finish; 
    end

endmodule

