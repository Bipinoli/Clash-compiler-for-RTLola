#!/bin/bash

# stack setup 
# stack install clash-ghc


echo "This was created to use the global installation of Clash with ~/.stack/global-project/stack.yaml."
echo "However, now I have created stack.yaml & cabal config locally following https://github.com/clash-lang/clash-starters/blob/main/simple/stack.yaml"
echo "So this is no longer needed"

echo "\nInorder to build locally simply do:"
echo "\tstack clean"
echo "\tstack build"

echo "\nTo run clashi do"
echo "\tstack exec -- clashi"

