#!/bin/sh

dir="src/evaluation/synthesis_hardware_components/specs/all_major_supported"

rm -rf generated
mkdir generated
cd ../../../../..
cargo run -- --spec "$dir/spec.lola" --output "$dir/generated" --vhdl
cd $dir/generated

chmod +x gen_vhdl.sh
./gen_vhdl.sh

cp ../../../new_compiler_synth_script.sh ./vhdl/Spec.topEntity/synth.sh