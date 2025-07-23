#!/bin/bash

ghdl --clean

ghdl -a --std=08 --work=work Spec_topEntity_types.vhdl
ghdl -a --std=08 --work=work topEntity.vhdl
ghdl -e --std=08 --work=work topEntity

yosys -m ghdl -p "
ghdl --latches --std=08 --work=work topEntity;
synth -top topEntity;
stat;
" > stats.txt