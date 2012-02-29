#!/bin/bash

FILE=testdata/ex2
TXT_IN=$FILE.txt
DOT_OUT=$FILE.dot
IMG_OUT=$FILE.png

pushd ./
cd src
ghc --make Main -outputdir ../build -o ../build/hafer
popd
./build/hafer --coupling $TXT_IN > $FILE-coupling.txt
./build/hafer --java $TXT_IN > $FILE-java.txt
./build/hafer --classdiagram $TXT_IN > $FILE-cladia.dot
dot -T png -o $FILE-cladia.png $FILE-cladia.dot
./build/hafer --graph $TXT_IN > $FILE-graph.dot
dot -T png -o $FILE-graph.png $FILE-graph.dot
