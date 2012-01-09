module Hafer.Data.GenericGraph 
( module Hafer.Data.GenericGraph -- TODO: proper interface 
) where

-- Data declarations
data Graph v e = CleanGraph [Vertex v] [Edge v e]
               | MessyGraph [GraphElem v e]
    deriving (Show, Eq)

-- Simple vertex: empty list
-- Vertex container/"Subgraph": non-empty list
data Vertex v = Vertex v [Vertex v]
    deriving (Show, Eq)

data Edge v e = Edge e (Vertex v) (Vertex v)
    deriving (Show, Eq)

data GraphElem v e = GVertex (Vertex v)
                   | GEdge   (Edge v e)
    deriving (Show, Eq)


-- Graph properties
vertices :: Graph v e -> [Vertex v]
vertices g = case g of
    CleanGraph vs es -> vs
    MessyGraph elems -> foldr (++) [] $ map (\e -> case e of
                                                    GVertex v -> [v]
                                                    _         -> []) elems 
edges :: Graph v e -> [Edge v e]
edges g = case g of
    CleanGraph vs es -> es
    MessyGraph elems -> foldr (++) [] $ map (\e -> case e of
                                                    GEdge e -> [e]
                                                    _       -> []) elems 

elements :: Graph v e -> [GraphElem v e]
elements g = case g of
    MessyGraph elems -> elems
    CleanGraph vs es -> map (\v -> GVertex v) vs ++ map (\e -> GEdge e) es 

-- Not needed anymore due to fold/map instead of map/filter 
-- implementation of above functions
{-
-- GraphElem properties
isVertex :: GraphElem v e -> Bool
isVertex ge = case ge of
    GVertex v -> True
    GEdge   e -> False

isEdge :: GraphElem v e -> Bool
isEdge ge = not $ isVertex ge 
-}

