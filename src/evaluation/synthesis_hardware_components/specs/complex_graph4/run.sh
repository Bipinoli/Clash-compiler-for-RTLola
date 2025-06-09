#!/bin/sh

dir="src/evaluation/synthesis_hardware_components/specs/complex_graph4"

rm -rf generated
mkdir generated
cd ../../../../..
cargo run -- --spec "$dir/spec.lola" --output "$dir/generated" --vhdl
cd $dir/generated

chmod +x gen_vhdl.sh
./gen_vhdl.sh

cp ../../../synth_template.ys ./vhdl/Spec.topEntity/synth.ys
