`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg input_a;
    reg new_input_a;
    reg input_b;
    reg new_input_b;
    reg signed [63:0] input_id;
    reg new_input_id;

    wire output_lt;
    wire output_gt;
    wire output_neq;
    wire output_not_a;
    wire output_a_impl_b;
    wire signed [63:0] output_time_stream;
 
    wire output_lt_aktv;
    wire output_gt_aktv;
    wire output_neq_aktv;
    wire output_not_a_aktv;
    wire output_a_impl_b_aktv;
    wire output_time_stream_aktv;   

    wire hlc_clock;
    wire signed [63:0] hlc_clock_cnt;
    wire hlc_a;
    wire hlc_b;
    wire signed [63:0] hlc_id;
    wire hlc_en_lt;
    wire hlc_en_gt;
    wire hlc_en_neq;
    wire hlc_en_not_a;
    wire hlc_en_a_impl_b;
    wire hlc_en_time_stream;

    wire signed [63:0] llc_stage;



    topEntity monitor (clk, rst, en,
                       input_a, new_input_a,
                       input_b, new_input_b,
                       input_id, new_input_id,
                       hlc_clock, hlc_clock_cnt,
                       hlc_a, hlc_b, hlc_id,
                       hlc_en_lt, hlc_en_gt, hlc_en_neq, hlc_en_not_a, hlc_en_a_impl_b, hlc_en_time_stream,
                       llc_stage,
                       output_lt, output_lt_aktv,
                       output_gt, output_gt_aktv,
                       output_neq, output_neq_aktv,
                       output_not_a, output_not_a_aktv,
                       output_a_impl_b, output_a_impl_b_aktv,
                       output_time_stream, output_time_stream_aktv
                       );

    always begin
        #1 clk = ~clk;
    end

    // LLC runs 5X faster than HLC so the whole monitor runs at 1/5th of the clock frequency
    // Note: It is important that the new data is provided at stage = 0
    initial begin
        clk = 0;
        rst = 0;
        en = 0;
        new_input_a = 0;
        new_input_b = 0;
        new_input_id = 0;
        
        $printtimescale(testbench);
        $dumpvars(0, testbench);

        // 5 cycles = 5 * #2;
        #10;
        input_a = 1;
        input_b = 1;
        input_id = 2;
        new_input_a = 1;
        new_input_b = 1;
        new_input_id = 1;
        en = 1;
        #1; 
        input_a = 0;
        input_b = 0;
        input_id = 0;
        new_input_a = 0;
        new_input_b = 0;
        new_input_id = 0;

        #500; // 0.5 ms

        // 5 cycles = 5 * #2;
        #10;
        input_a = 1;
        input_b = 0;
        input_id = 3;
        new_input_a = 1;
        new_input_b = 1;
        new_input_id = 1;
        en = 1;
        #1; 
        input_a = 0;
        input_b = 0;
        input_id = 0;
        new_input_a = 0;
        new_input_b = 0;
        new_input_id = 0;

        #500; // 0.5 ms
        
        // 5 cycles = 5 * #2;
        #10;
        input_a = 0;
        input_b = 0;
        input_id = 4;
        new_input_a = 1;
        new_input_b = 0;
        new_input_id = 1;
        en = 1;
        #1; 
        input_a = 0;
        input_b = 0;
        input_id = 0;
        new_input_a = 0;
        new_input_b = 0;
        new_input_id = 0;

        #500; // 0.5 ms

        
        #12;
        $finish; 
    end

endmodule

