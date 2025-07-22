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
ghdl -a --std=08 --work=work llc/a_output_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/f_output_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/c_output_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/evaluator.vhdl
ghdl -a --std=08 --work=work llc/d_output_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/low_level_controller.vhdl
ghdl -a --std=08 --work=work llc/e_output_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/y_input_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/b_output_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/x_input_stream_entity.vhdl
ghdl -a --std=08 --work=work llc/g_output_stream_entity.vhdl
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
