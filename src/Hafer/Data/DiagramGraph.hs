module Hafer.Data.DiagramGraph 
( module Hafer.Data.DiagramGraph
, module Hafer.Data.ClassDiagram
) where

import Data.Char
import Hafer.Data.ClassDiagram

data DiagramGraph = DiagramGraph [Node] [Edge]
    deriving (Show)

data GraphElem = Node Node
               | Edge Edge
    deriving (Show)

data Node = Label String
          | Class Name [Field] [Method]
          | Pkg [Node]
    deriving (Show, Eq)

data Edge = Simple Node Node
          | Association Node Node [AssociationProp]
    deriving (Show, Eq)

add :: DiagramGraph -> GraphElem -> DiagramGraph
add g a = case a of 
    Node n -> case g of
              DiagramGraph ns es -> DiagramGraph (ns ++ [n]) es
    Edge e -> case g of
              DiagramGraph ns es -> DiagramGraph ns (es ++ [e])

-- testAdd = add (add (Graph [] []) (Node $ Class "foo" [])) (Node $ Class "bar" [])
