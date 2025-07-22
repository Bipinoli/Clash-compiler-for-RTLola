#!/bin/bash

ghdl --clean

ghdl -a --std=08 --work=work my_array_package.vhdl
ghdl -a --std=08 --work=work my_math_package.vhdl
ghdl -a --std=08 --work=work hlc/hl_qinterface.vhdl
ghdl -a --std=08 --work=work hlc/high_level_controller.vhdl
ghdl -a --std=08 --work=work hlc/check_new_input.vhdl
ghdl -a --std=08 --work=work hlc/event_delay.vhdl
ghdl -a --std=08 --work=work hlc/scheduler.vhdl
ghdl -a --std=08 --work=work hlc/extInterface.vhdl
ghdl -a --std=08 --work=work hlc/time_unit.vhdl
ghdl -a --std=08 --work=work llc/num_satellites_input_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/few_satellites_count_1_sliding_window_entity.vhdl
ghdl -a --std=08 --work=work llc/imu_acc_x_input_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/evaluator.vhdl
ghdl -a --std=08 --work=work llc/gps_emitted_enough_output_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/low_level_controller.vhdl
ghdl -a --std=08 --work=work llc/few_satellites_output_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/is_unreliable_gps_data_output_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/gps_x_input_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/gps_x_count_0_sliding_window_entity.vhdl
ghdl -a --std=08 --work=work queue/queue.vhdl
ghdl -a --std=08 --work=work monitor.vhdl
ghdl -a --std=08 --work=work pre_processing/input_pre_processing.vhdl
ghdl -a --std=08 --work=work pre_processing/clock_pre_processing.vhdl
ghdl -a --std=08 --work=work implementation.vhdl
ghdl -e --std=08 --work=work implementation

yosys -m ghdl -p "
ghdl --latches --std=08 --work=work implementation; 
synth -top implementation; 
stat;
" > stats.txt
