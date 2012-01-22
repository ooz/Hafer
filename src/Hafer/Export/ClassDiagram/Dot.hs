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

testClass = Class (Name "Foobar") [Field VisPrivate "foo" (Type "int")] [Method VisPublic "bar" [("baz",(Type "int"))] (Type "void")]
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

escape :: String -> String
escape name = escapeReserved $ map escapeChar name 

escapeChar :: Char -> Char
escapeChar c = case c of
    ' ' -> '_'
    '<' -> '_'
    '>' -> '_'
    ',' -> '_'
    '.' -> '_'
    '\\'-> '_'
    _   -> c

escapeReserved :: String -> String
escapeReserved word = case (elem word reservedWords) of
    True  -> "_" ++ word
    False -> word

convertVertex :: Vertex CDNode -> String
convertVertex v = case v of
    Vertex (Class name fields methods) [] -> 
        let name' = convertName name
        in 
            (escape name') ++ " [ label = \"{" 
                           ++ name'
                           ++ (addFields fields) 
                           ++ (addMethods methods) 
                           ++ "}\" ]"  
    _ -> ""


convertName :: Name -> String
convertName n = case n of
    Name nn                -> nn
    QualifiedName qs n'    -> qs ++ "." ++ convertName n'
    ParametrizedName nn [] -> nn 
    ParametrizedName nn ps -> nn ++ "\\<" ++ (reduceSep ps ", ")  ++ "\\>"

reduceSep :: [String] -> String -> String
reduceSep (l:ls) s = foldl (\a b -> a ++ s ++ b) l ls

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
    Dynamic -> ""
    _ -> " : " ++ convertType t

convertType :: Type -> String
convertType t = case t of
    Type name               -> name
    PolymorphicType name ts -> name ++ "\\<" ++ convertTypes ts ++ "\\>"
    _ -> ""

convertTypes :: [Type] -> String
convertTypes ts = case (map (\a -> convertType a) ts) of
    t:ts' -> reduceSep (t:ts') ", "
    []    -> ""

convertParams :: [Param] -> String
convertParams l = case (map (\a -> convertParam a) l) of
    p:ps -> reduceSep (p:ps) ", "
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
convertArrow d l r = let l' = escape (extractNodeName l)
                         r' = escape (extractNodeName r)
                     in case d of
                        L2R -> l' ++ " -> " ++ r'
                        R2L -> r' ++ " -> " ++ l'
                        _   -> l' ++ " -> " ++ r'

extractNodeName :: Vertex CDNode -> String
extractNodeName (Vertex n _) = case n of
    Class name _ _   -> convertName name
    Interface name _ -> convertName name
    Package name     -> convertName name
