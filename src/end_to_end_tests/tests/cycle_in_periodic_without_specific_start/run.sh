#!/bin/sh

test_dir="src/end_to_end_tests/tests/cycle_in_periodic_without_specific_start"

rm -rf generated
mkdir generated
cd ../../../..
cargo run -- --spec "$test_dir/spec.lola" --output "$test_dir/generated" --trace "$test_dir/trace.csv" --all --debug
cd $test_dir/generated

chmod +x gen_verilog.sh
chmod +x dump_waveform.sh

./gen_verilog.sh
./dump_waveform.sh >output.txt
