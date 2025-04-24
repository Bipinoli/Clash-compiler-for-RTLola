#!/bin/sh

rm -f a.out &&
iverilog -o a.out ./verilog/Debug.topEntity/topEntity.v testbench.v &&
vvp a.out &&
gtkwave dump.vcd

