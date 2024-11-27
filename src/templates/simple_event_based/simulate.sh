#!/bin/bash

rm -f a.out &&
iverilog -o a.out ../verilog/SimpleEventBased.topEntity/topEntity.v testbench.v &&
vvp a.out
