`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] input_a;
    reg new_input;

    wire signed [63:0] hlc_a;
    wire hlc_enB;
    wire hlc_enC;
    wire hlc_enD;
    wire hlc_clock;

    wire signed [63:0] llc_stage;
    wire signed [63:0] output_b;
    wire signed [63:0] output_c;
    wire signed [63:0] output_d;
    wire output_b_aktv;
    wire output_c_aktv;
    wire output_d_aktv;
    wire signed [63:0] b_timer;
    wire signed [63:0] c_timer;
    wire signed [63:0] d_timer;
    wire b_timer_rst;
    wire c_timer_rst;
    wire d_timer_rst;

    topEntity monitor (clk, rst, en,
                       input_a, new_input,
                       hlc_clock, hlc_a, hlc_enB, hlc_enC, hlc_enD, b_timer, c_timer, d_timer, b_timer_rst, c_timer_rst, d_timer_rst,
                       llc_stage,
                       output_b, output_b_aktv,
                       output_c, output_c_aktv,
                       output_d, output_d_aktv
                       );

    always begin
        #1 clk = ~clk;
    end

    always @(llc_stage) begin
        $display("time: %0t, llc_stage: %0d, (a, newA): (%0d, %0d), output_aktv: (%0d, %0d, %0d), b: %0d, c: %0d, d: %0d",
         $time, llc_stage, input_a, new_input, 
         output_b_aktv, output_c_aktv, output_d_aktv, 
         output_b, output_c, output_d);
    end

    // LLC runs 4X faster than HLC so the whole monitor runs at 1/4th of the clock frequency
    // Note: It is important that the new data is provided at stage = 0
    initial begin
        clk = 0;
        rst = 0;
        en = 0;
        new_input = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        // 4 cycles = 4 * #2;
        #8;
        input_a = 1;
        new_input = 1;
        en = 1;
        #1; new_input = 0;

        #100; // 0.1 seconds
        
        #8;
        input_a = 2;
        new_input = 1;
        #1; new_input = 0;

        #100; // 0.1 seconds

        #8;
        input_a = 3;
        new_input = 1;
        #1; new_input = 0;
        
        #100; // 0.1 seconds

        #8;
        input_a = 4;
        new_input = 1;
        #1; new_input = 0;
        
        #100; // 0.1 seconds

        #8;
        input_a = 5;
        new_input = 1;
        #1; new_input = 0;
        
        #100; // 0.1 seconds

        #8;
        input_a = 6;
        new_input = 1;
        #1; new_input = 0;
        
        #100; // 0.1 seconds

        #8;
        input_a = 7;
        new_input = 1;
        #1; new_input = 0;
        
        #100; // 0.1 seconds

        #8;
        input_a = 8;
        new_input = 1;
        #1; new_input = 0;
        
        #100; // 0.1 seconds

        #8;
        input_a = 9;
        new_input = 1;
        #1; new_input = 0;
        
        #100; // 0.1 seconds

        #8;
        input_a = 10;
        new_input = 1;
        #1; new_input = 0;
        
        #100; // 0.1 seconds

        #12;
        $finish; 
    end

endmodule

