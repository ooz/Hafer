{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hafer.Export.ClassDiagram.Dot 
( ExportMethod
, exprt
) where

import Hafer.Export.Common.Dot ( cGRAPH_START
                               , cGRAPH_END
                               , cGENERAL_CONFIG
                               , reservedWords
                               , escape
                               , escapeChar
                               , escapeReserved
                               )
import Hafer.Export.ExportMethod
import Hafer.Data.ClassDiagram

instance ExportMethod CDGraph String where
    exprt g = export g



-- ##########################################################################
-- # Additional boilerplate
-- ##########################################################################

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



-- ##########################################################################
-- # Test
-- ##########################################################################

testClass = Class (Name "Foobar") [Field VisPrivate "foo" (Type "int")] [Method VisPublic "bar" [("baz",(Type "int"))] (Type "void")]
testGraph = MathGraph [Vertex testClass] []

testConvertNode = convertVertex testGraph $ Vertex testClass 

-- ##########################################################################
-- # Export
-- ##########################################################################

export :: CDGraph -> String
export g = let vs = vertices g
               es = edges g
               nonPkgEdges = filter (\e -> case e of
                                            Edge PkgContain _ _ _ -> False
                                            _ -> True
                                    ) es
           in
                cGRAPH_START ++ "\n"
                ++ cGENERAL_CONFIG ++ "\n"
                ++ _NODE_CONFIG ++ "\n"
                ++ _EDGE_CONFIG ++ "\n"
                ++ (foldr (++) "" (map (\v -> convertVertex g v ++ "\n") vs))
                ++ (foldr (++) "" (map (\e -> convertEdge e ++ "\n"  ) nonPkgEdges))
                ++ cGRAPH_END ++ "\n"



-- ##########################################################################
-- # Vertex conversion
-- ##########################################################################

convertVertex :: CDGraph -> Vertex CDNode -> String
convertVertex g v = case v of
    Vertex (Package name) ->
        let nameStr = convertName name
            adjas   = adjacents g v
            -- TODO: build package hierachy!
            children = filter (\a -> case a of
                                        Vertex (Class n fs ms) -> True
                                        _ -> False
                              ) adjas
        in  "subgraph cluster" ++ (escape nameStr)
                ++ " {"
                ++ " label = \"" ++ nameStr ++ "\"\n"
                ++ (foldr (++) "" (map (\v -> convertVertex g v ++ "\n") children)) 
                ++  "}"
--     Vertex (Package name) children ->
--         let name' = convertName name
--         in 
--             "subgraph cluster" ++ (escape name') 
--                 ++ " {" 
--                 ++ " label = \"" ++ name' ++ "\"\n"
--                 ++ (foldr (++) "" (map (\v -> convertVertex v ++ "\n") children)) 
--                 ++  "}"
    Vertex (Class name fields methods) -> 
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
    ParametrizedName nn [] -> nn 
    ParametrizedName nn ps -> nn ++ "\\<" ++ (reduceSep ps ", ")  ++ "\\>"
    _                      -> format n

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



-- ##########################################################################
-- # Edge conversion
-- ##########################################################################

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
extractNodeName (Vertex n) = case n of
    Class name _ _   -> convertName name
    Interface name _ -> convertName name
    Package name     -> convertName name
