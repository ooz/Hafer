#!/bin/bash

FILE=testdata/ex2
TXT_IN=$FILE.txt
DOT_OUT=$FILE.dot
IMG_OUT=$FILE.png

pushd ./
cd src
ghc --make Main -outputdir ../build -o ../build/hafer
popd
./build/hafer $TXT_IN > $DOT_OUT
dot -T png -o $IMG_OUT $DOT_OUT
