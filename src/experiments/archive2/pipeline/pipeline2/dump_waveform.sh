#!/bin/sh

rm -f a.out &&
iverilog -o a.out ./verilog/Pipeline2.topEntity/topEntity.v testbench.v &&
vvp a.out

