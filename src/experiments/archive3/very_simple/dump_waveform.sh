#!/bin/sh

rm -f a.out &&
iverilog -o a.out ./verilog/VerySimple.topEntity/topEntity.v testbench.v &&
vvp a.out
