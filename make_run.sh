#!/bin/bash

DOT_OUT=model_stripped.dot

ghc --make Main
./Main model_stripped.txt > $DOT_OUT
dot -T png -o model_stripped.png $DOT_OUT
