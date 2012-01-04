#!/bin/bash

FILE=test/data/test
TXT_IN=$FILE.txt
DOT_OUT=$FILE.dot
IMG_OUT=$FILE.png

ghc --make Main
./Main $TXT_IN > $DOT_OUT
dot -T png -o $IMG_OUT $DOT_OUT
