#!/bin/sh

dir="src/evaluation/synthesis_hardware_components/specs/cycle_cant_pipeline"

rm -rf generated
mkdir generated
cd ../../../../..
cargo run -- --spec "$dir/spec.lola" --output "$dir/generated" --vhdl
cd $dir/generated

chmod +x gen_vhdl.sh
./gen_vhdl.sh

cp ../../../synth_script_our_compiler.ys ./vhdl/Spec.topEntity/synth.ys