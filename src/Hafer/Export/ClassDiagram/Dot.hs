{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Hafer.Export.ClassDiagram.Dot 
( ExportMethod
, exprt
) where

import Hafer.Export.ExportMethod
import Hafer.Data.ClassDiagram

instance ExportMethod CDGraph String where
    exprt g = export g

_GRAPH_START = "digraph G {"
_GRAPH_END   = "}"

_GENERAL_CONFIG = "fontname = \"Bitstream Vera Sans\"\
\ rankdir = \"RL\"\
\ fontsize = 8"

_NODE_CONFIG = "node [\
\ fontname = \"Bitstream Vera Sans\"\
\ fontsize = 8\
\ shape = \"record\"\
\ ]"

_EDGE_CONFIG = "edge [\
\ fontname = \"Bitstream Vera Sans\"\
\ fontsize = 8\
\ ]"

_EDGE_EXTEND_CONFIG = "edge [ arrowhead = \"empty\" ]"

testClass = Class "Foobar" [Field VisPrivate "foo" (SimpleType "int")] [Method VisPublic "bar" [("baz",(SimpleType "int"))] (SimpleType "void")]
testGraph = MathGraph [Vertex testClass []] []

testConvertNode = convertVertex $ Vertex testClass []

export :: CDGraph -> String
export g = let vs = vertices g
               es = edges g
           in
                _GRAPH_START ++ "\n"
                ++ _GENERAL_CONFIG ++ "\n"
                ++ _NODE_CONFIG ++ "\n"
                ++ _EDGE_CONFIG ++ "\n"
                ++ (foldr (++) "" (map (\v -> convertVertex v ++ "\n") vs))
                ++ (foldr (++) "" (map (\e -> convertEdge e ++ "\n"  ) es))
                ++ _GRAPH_END ++ "\n"

reservedWords = ["Graph", "Node", "Edge"]

escapeReserved :: String -> String
escapeReserved word = case (elem word reservedWords) of
    True  -> "_" ++ word
    False -> word

convertVertex :: Vertex CDNode -> String
convertVertex v = case v of
    Vertex (Class name fields methods) [] -> 
        (escapeReserved name) ++ " [ label = \"{" 
                              ++ name 
                              ++ (addFields fields) 
                              ++ (addMethods methods) 
                              ++ "}\" ]"  
    _ -> ""


addFields :: [Field] -> String
addFields l = case l of 
    a:as -> "|" ++ convertFields l
    []   -> ""
convertFields :: [Field] -> String
convertFields fields = case fields of
    (Field v name t):fs -> (convertVisibility v) 
                           ++ name 
                           ++ (convertOptType t) 
                           ++ "\\l" 
                           ++ convertFields fs
    []   -> ""

addMethods :: [Method] -> String
addMethods l = case l of 
    a:as -> "|" ++ convertMethods l
    []   -> ""
convertMethods :: [Method] -> String
convertMethods methods = case methods of
    (Method v name params t):ms -> (convertVisibility v)
                                   ++ name 
                                   ++ "(" ++ (convertParams params) ++ ")"
                                   ++ (convertOptType t)
                                   ++ "\\l"
                                   ++ convertMethods ms
    []   -> ""

convertVisibility :: Visibility -> String
convertVisibility v = case v of
    VisPrivate -> "- "
    VisPackage -> "~ "
    VisPublic  -> "+ "
    _          -> ""

convertOptType :: Type -> String
convertOptType t = case t of
    TypeNotSpecified -> ""
    _ -> " : " ++ convertType t

convertType :: Type -> String
convertType t = case t of
    SimpleType name         -> name
    PolymorphicType name ts -> name ++ "\\<" ++ convertTypes ts ++ "\\>"
    _ -> ""

convertTypes :: [Type] -> String
convertTypes ts = case (map (\a -> convertType a) ts) of
    t:ts' -> foldl (\a b -> a ++ ", " ++ b) t ts'
    []    -> ""

convertParams :: [Param] -> String
convertParams l = case l of
    p:ps -> foldl (\a b -> a ++ ", " ++ (convertParam b)) (convertParam p) ps
    []   -> ""

convertParam :: Param -> String
convertParam (n,t) = n ++ convertType t



-- # Edge conversion

convertEdge :: Edge CDNode CDAssoc -> String
convertEdge e = case e of
    Edge assoc dir l r -> (convertEdgeType assoc) ++ "\n"
                          ++ (convertArrow dir l r)

convertEdgeType :: CDAssoc -> String
convertEdgeType a = case a of
    Extend -> _EDGE_EXTEND_CONFIG

convertArrow :: Direction -> Vertex CDNode -> Vertex CDNode -> String
convertArrow d l r = let l' = escapeReserved (extractNodeName l)
                         r' = escapeReserved (extractNodeName r)
                     in case d of
                        L2R -> l' ++ " -> " ++ r'
                        R2L -> r' ++ " -> " ++ l'
                        _   -> l' ++ " -> " ++ r'

extractNodeName :: Vertex CDNode -> String
extractNodeName (Vertex n _) = case n of
    Class name _ _   -> name
    Interface name _ -> name
    Package name     -> name
