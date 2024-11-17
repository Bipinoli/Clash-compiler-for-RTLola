#!/bin/bash

iverilog -o a.out ./verilog/DiffFreqsSimple.topEntity/topEntity.v test_generated_diff_freqs_simple.v
vvp a.out
