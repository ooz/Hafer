{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Hafer.Export.Graph.Dot
( exprt
, export
) where

import Hafer.Export.Common.Dot ( cGRAPH_START
                               , cGRAPH_END
                               , cGENERAL_CONFIG
                               , escape
                               )
-- import Hafer.Export.ExportMethod
import Hafer.Data.GenericGraph

-- ##########################################################################
-- # Graphviz configuration boilerplate
-- ##########################################################################

_NODE_CONFIG = "node [\
\ fontname = \"Bitstream Vera Sans\"\
\ fontsize = 8\
\ shape = \"circle\"\
\ ]"

_EDGE_CONFIG = "edge [\ 
\ arrowhead = \"normal\"\
\ fontname = \"Bitstream Vera Sans\"\
\ fontsize = 8\
\ ]"

exprt :: (Format v, Format e) => 
         (Graph v e) -> String
exprt g = export g

export :: (Format v, Format e) => 
          (Graph v e) -> String
export g = let
              vs = vertices g
              es = edges g
           in 
              cGRAPH_START ++ "\n"
              ++ cGENERAL_CONFIG ++ "\n"
              ++ _NODE_CONFIG ++ "\n"
              ++ _EDGE_CONFIG ++ "\n"
              ++ (foldr (++) "" (map (\v -> convertVertex v ++ "\n") vs))
              ++ (foldr (++) "" (map (\e -> convertEdge   e ++ "\n") es))
              ++ cGRAPH_END ++ "\n"

convertVertex :: Format a => 
                 Vertex a -> String
convertVertex (Vertex a) = let 
                            aLabel = format a
                           in
                            (escape aLabel) ++ " [ label = \""
                                            ++ aLabel
                                            ++ "\" ]"

convertEdge :: (Format v, Format e) => 
               (Edge v e) -> String
convertEdge (Edge a dir (Vertex l) (Vertex r)) 
                             = let
                                ln = escape $ format l
                                rn = escape $ format r
                               in case dir of
                                L2R -> ln ++ " -> " ++ rn
                                R2L -> rn ++ " -> " ++ ln
                                _   -> ln ++ " -> " ++ rn
                                
