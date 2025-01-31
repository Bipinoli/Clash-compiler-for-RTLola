`timescale 1us/1ns
module testbench;
    reg clk;
    reg rst;
    reg en;

    reg signed [63:0] input_a;
    reg new_input;

    wire hlc_clock;
    wire signed [63:0] hlc_clock_cnt;
    wire signed [63:0] bTimer;
    wire bEn;
    wire bTimerRst;
    wire signed [63:0] bTimerCurTime;
    wire curEn;

    topEntity monitor (clk, rst, en,
                       input_a, new_input,
                       hlc_clock, hlc_clock_cnt,
                       bEn, bTimer, bTimerRst, bTimerCurTime, curEn
                       );

    always begin
        #1 clk = ~clk;
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

