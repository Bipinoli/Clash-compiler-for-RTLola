`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] input_x;
    reg new_input;

    wire signed [63:0] hlc_x;
    wire hlc_enA;
    wire hlc_enB;
    wire hlc_enC;
    wire hlc_clock;

    wire signed [63:0] llc_stage;
    wire signed [63:0] output_a;
    wire signed [63:0] output_b;
    wire signed [63:0] output_c;
    wire output_a_aktv;
    wire output_b_aktv;
    wire output_c_aktv;

    topEntity monitor (clk, rst, en,
                       input_x, new_input,
                       hlc_clock, hlc_x, hlc_enA, hlc_enB, hlc_enC,
                       llc_stage,
                       output_a, output_a_aktv,
                       output_b, output_b_aktv,
                       output_c, output_c_aktv
                       );

    always begin
        #1 clk = ~clk;
    end

    always @(llc_stage) begin
        $display("time: %0t, llc_stage: %0d, (x, newX): (%0d, %0d), output_aktv: (%0d, %0d, %0d), a: %0d, b: %0d, c: %0d",
         $time, llc_stage, input_x, new_input, 
         output_a_aktv, output_b_aktv, output_c_aktv, 
         output_a, output_b, output_c);
    end

    // LLC runs 5X faster than HLC so the whole monitor runs at 1/5th of the clock frequency
    // Note: It is important that the new data is provided at stage = 0
    initial begin
        clk = 0;
        rst = 0;
        en = 0;
        new_input = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        // 5 cycles = 5 * #2;
        #10;
        input_x = 1;
        new_input = 1;
        en = 1;
        
        #10;
        input_x = 2;

        #10;
        input_x = 3;

        #10;
        input_x = 4;

        #10;
        input_x = 5;

        #10;
        input_x = 6;

        #12;
        $finish; 
    end

endmodule

