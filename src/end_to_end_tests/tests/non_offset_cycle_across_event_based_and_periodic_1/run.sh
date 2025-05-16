#!/bin/sh

test_dir="src/end_to_end_tests/tests/non_offset_cycle_across_event_based_and_periodic_1"

rm -rf generated
mkdir generated
cd ../../../..
cargo run -- --spec "$test_dir/spec.lola" --output "$test_dir/generated" --trace "$test_dir/trace.csv" --all --debug
cd $test_dir/generated

chmod +x gen_verilog.sh
chmod +x dump_waveform.sh

./gen_verilog.sh
./dump_waveform.sh >output.txt
