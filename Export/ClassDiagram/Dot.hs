module Export.ClassDiagram.Dot 
( export 
) where

import Data.DiagramGraph

_GRAPH_START = "digraph G {"
_GRAPH_END   = "}"

_GENERAL_CONFIG = "fontname = \"Bitstream Vera Sans\"\
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

testClass = Class "Foobar" [Field Private "foo" "int"] [Method Public "bar" [("baz","int")] "void"]
testGraph = DiagramGraph [testClass] []

export :: DiagramGraph -> String
export g = case g of
    DiagramGraph nodes edges -> 
        _GRAPH_START ++ "\n"
        ++ _GENERAL_CONFIG ++ "\n"
        ++ _NODE_CONFIG ++ "\n"
        ++ _EDGE_CONFIG ++ "\n"
        ++ (foldr (++) "" (map (\n -> convertNode n ++ "\n") nodes))
        ++ _GRAPH_END ++ "\n"

testConvertNode = convertNode $ testClass

reservedWords = ["Graph", "Node", "Edge"]

escapeReserved :: String -> String
escapeReserved word = case (elem word reservedWords) of
    True  -> "_" ++ word
    False -> word

convertNode :: Node -> String
convertNode n = case n of
    Class name fields methods -> 
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
                           ++ (convertType t) 
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
                                   ++ (convertType t)
                                   ++ "\\l"
                                   ++ convertMethods ms
    []   -> ""

convertVisibility :: Visibility -> String
convertVisibility v = case v of
    Private -> "- "
    Package -> "~ "
    Public  -> "+ "
    _       -> ""

convertType :: String -> String
convertType t = case t of
    "" -> ""
    _  -> " : " ++ t

convertParams :: [Param] -> String
convertParams l = case l of
    p:ps -> foldl (\a b -> a ++ ", " ++ (convertParam b)) (convertParam p) ps
    []   -> ""

convertParam :: Param -> String
convertParam (n,t) = n ++ convertType t
