#!/bin/sh

test_dir="src/temp"

rm -rf generated
mkdir generated
cd ../..
cargo run -- --spec "$test_dir/spec.lola" --output "$test_dir/generated" --verilog
cd $test_dir/generated

chmod +x gen_verilog.sh
# chmod +x dump_waveform.sh

./gen_verilog.sh
# ./dump_waveform.sh >output.txt
