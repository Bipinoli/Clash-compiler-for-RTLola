#!/bin/sh

rm -f a.out &&
iverilog -o a.out ./verilog/CycleWithTrickyOffset.topEntity/topEntity.v testbench.v &&
vvp a.out
gtkwave dump.vcd

