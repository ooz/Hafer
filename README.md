# Hafer

Hafer is a text based diagram renderer (Graphviz backend) and code generator written in Haskell. 
It is heavily inspired by yuml.me

[Class Diagram syntax](https://github.com/ooz/Hafer/blob/master/doc/model.txt)

# Example

## Class Diagram

[Hafer data model visualized using itself:](https://github.com/ooz/Hafer/blob/master/testdata/ex2.txt)
![alt](https://raw.githubusercontent.com/ooz/Hafer/master/testdata/ex2-cladia.png)

## Graph derived Coupling metric

[](https://github.com/ooz/Hafer/blob/master/testdata/ex2-coupling.txt)

## Generated Java source

[](https://github.com/ooz/Hafer/blob/master/testdata/ex2-java.txt)

# Dependencies

 * graphviz 2.26.x (older versions may work too)

## cabal packages

 * parsec
 * missingh

# TODO

 * make cabal package ;)
 * use Haskell graphviz bindings
