# Hafer is deprecated - use [mermaid](https://github.com/knsv/mermaid) instead to render diagrams

Hafer is a text based diagram renderer (Graphviz backend) and code generator written in Haskell. 
It is heavily inspired by yuml.me

[Class Diagram syntax](https://github.com/ooz/Hafer/blob/master/doc/model.txt)

## Example

[Hafer data model class diagram (source)](https://github.com/ooz/Hafer/blob/master/testdata/ex2.txt)

![Hafer data model visualized using itself](https://raw.githubusercontent.com/ooz/Hafer/master/testdata/ex2-cladia.png)

[Graph derived Coupling metric](https://github.com/ooz/Hafer/blob/master/testdata/ex2-coupling.txt)

[Generated Java source](https://github.com/ooz/Hafer/blob/master/testdata/ex2-java.txt)

## Dependencies

 * graphviz 2.26.x (older versions may work too)

### cabal packages

 * parsec
 * missingh

