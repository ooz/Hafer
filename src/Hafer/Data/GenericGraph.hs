module Hafer.Data.GenericGraph 
( module Hafer.Data.GenericGraph -- TODO: proper interface 
) where

-- Data declarations
data Graph v e = MathGraph    [Vertex v] [Edge v e]
               | ElemSetGraph [GraphElem v e]
    deriving (Show, Eq)

-- Simple vertex: empty list
-- Vertex container/"Subgraph": non-empty list
data Vertex v = Vertex v [Vertex v]
    deriving (Show, Eq)

data Edge v e = Edge e Direction (Vertex v) (Vertex v)
    deriving (Show, Eq)

data GraphElem v e = GVertex (Vertex v)
                   | GEdge   (Edge v e)
    deriving (Show, Eq)

data Direction = None
               | L2R
               | R2L
               | Both
    deriving (Show, Eq)


-- Graph properties
vertices :: Graph v e -> [Vertex v]
vertices g = case g of
    MathGraph vs es    -> vs
    ElemSetGraph elems -> foldr (++) [] $ map (\e -> case e of
                                                      GVertex v -> [v]
                                                      _         -> []) elems 
edges :: Graph v e -> [Edge v e]
edges g = case g of
    MathGraph vs es    -> es
    ElemSetGraph elems -> foldr (++) [] $ map (\e -> case e of
                                                      GEdge e -> [e]
                                                      _       -> []) elems 

elements :: Graph v e -> [GraphElem v e]
elements g = case g of
    ElemSetGraph elems -> elems
    MathGraph vs es    -> map (\v -> GVertex v) vs ++ map (\e -> GEdge e) es 

