module Hafer.Data.GenericGraph 
( module Hafer.Data.GenericGraph -- TODO: proper interface 
) where

import Data.List (find)

-- Data declarations
data Graph v e = MathGraph    [Vertex v] [Edge v e]
               | ElemSetGraph [GraphElem v e]
    deriving (Show, Eq)

data Vertex v = Vertex v 
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


-- | Typeclass for converting vertex/edge types to labels that can be
--   shown in a generic graph rendering.
--   Reduced version of Text.Show typeclass 
--   (which should rather be used for debugging purposes)
class Format a where
    format :: a -> String


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

edgesFor :: Eq v => 
            Graph v e -> Vertex v -> [Edge v e]
edgesFor g v = let es = edges g
               in  filter (isConnectedVia v) es

isConnectedVia :: Eq v => 
                  Vertex v -> Edge v e -> Bool
isConnectedVia v (Edge e dir a b) =
    if (v == a) || (v == b)
    then True
    else False

adjacents :: Eq v => 
             Graph v e -> Vertex v -> [Vertex v]
adjacents g v = let es = edgesFor g v
                    vs = vertices g
                    adjaRefs = map ( \e -> case e of
                                            Edge e' dir a b -> if a == v 
                                                               then b
                                                               else a
                                   ) 
                                   es
                in  map (\ref -> case (find (\v -> v == ref) vs) of
                                  Just v  -> v
                                  Nothing -> ref
                        )
                        adjaRefs

-- ##########################################################################
-- # Graph properties
-- ##########################################################################

degree :: Eq v => Graph v e -> [(Vertex v, Int)]
degree g = let 
            vs = vertices g
           in map (\v -> let 
                           adjas = adjacents g v
                         in
                           (v, length adjas)
                  ) vs

maxDegree :: Eq v => Graph v e -> Int
maxDegree g = maximum $ map (\(v, d) -> d) $ degree g
