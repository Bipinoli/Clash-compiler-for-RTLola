#!/bin/sh

rm -f a.out &&
iverilog -o a.out ./verilog/CycleComplexLoop.topEntity/topEntity.v testbench.v &&
vvp a.out

