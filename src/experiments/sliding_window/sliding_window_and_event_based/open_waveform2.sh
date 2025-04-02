#!/bin/sh

rm -f a.out &&
iverilog -o a.out ./verilog/SlidingWindowSimple.topEntity/topEntity.v testbench2.v &&
vvp a.out && 
gtkwave dump.vcd

