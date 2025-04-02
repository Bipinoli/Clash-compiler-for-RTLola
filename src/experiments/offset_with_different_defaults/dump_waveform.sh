#!/bin/sh

rm -f a.out &&
iverilog -o a.out ./verilog/OffsetWithDiffDefaults.topEntity/topEntity.v testbench.v &&
vvp a.out

