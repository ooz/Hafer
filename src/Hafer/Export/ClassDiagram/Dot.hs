{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hafer.Export.ClassDiagram.Dot 
--( ExportMethod
--, exprt
( exprt
, export
) where

import Data.List (find)

import Hafer.Export.Common.Dot ( cGRAPH_START
                               , cGRAPH_END
                               , cGENERAL_CONFIG
                               , reservedWords
                               , escape
                               , escapeChar
                               , escapeReserved
                               )
-- import Hafer.Export.ExportMethod
import Hafer.Data.ClassDiagram

-- instance ExportMethod CDGraph String where
--     exprt g = export g



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

_EDGE_EXTEND_CONFIG = "edge [ arrowhead = \"empty\" arrowtail = \"none\" ]"
_EDGE_AGGREGATION_CONFIG = "edge [ arrowhead = \"vee\" arrowtail = \"odiamond\" dir=both ]"
_EDGE_COMPOSITION_CONFIG = "edge [ arrowhead = \"vee\" arrowtail = \"diamond\" dir=both ]"



-- ##########################################################################
-- # Test
-- ##########################################################################

testClass = Class (Name "Foobar") [Field VisPrivate "foo" (Type "int")] [Method VisPublic "bar" [("baz",(Type "int"))] (Type "void")]
testGraph = MathGraph [Vertex testClass] []

testConvertNode = convertVertex (Name "") testGraph $ Vertex testClass 

-- ##########################################################################
-- # Export
-- ##########################################################################

exprt :: CDGraph -> String
exprt g = export g

export :: CDGraph -> String
export g = let vs = vertices g
               es = edges g
               nonPkgEdges = filter (\e -> case e of
                                            Edge PkgContain _ _ _ -> False
                                            _ -> True
                                    ) es
               pkgVerts = filter (\v -> case v of 
                                         Vertex (Package _) -> True
                                         _ -> False
                                 ) vs
               nonContainedVerts = filter (\v -> case v of
                                                  Vertex n -> 
                                                    case (find (\e -> case e of
                                                                       (Edge t dir (Vertex a) (Vertex b)) -> 
                                                                            case t of
                                                                             PkgContain | (a == n) || (b == n) -> True
                                                                             _ -> False
                                                         ) es) of
                                                      Nothing -> True
                                                      Just _  -> False
                                          ) vs
                                         
           in
                cGRAPH_START ++ "\n"
                ++ cGENERAL_CONFIG ++ "\n"
                ++ _NODE_CONFIG ++ "\n"
                ++ _EDGE_CONFIG ++ "\n"
                ++ (foldr (++) 
                          "" 
                          (map (\v -> convertVertex (Name "") g v ++ "\n") 
                               $ pkgVerts ++ nonContainedVerts))
                ++ (foldr (++) 
                          "" 
                          (map (\e -> convertEdge e ++ "\n"  ) 
                               nonPkgEdges))
                ++ cGRAPH_END ++ "\n"


-- buildPackageHierarchy :: [Vertex CDNode] -> Forest (Vertex CDNode)
-- buildPackageHierarchy l = case l of
--     [] -> []
--     v@(Vertex p@(Package name)):vs -> undefined
--     v@(_):vs -> [Node v []] ++ buildPackageHierarchy vs


-- ##########################################################################
-- # Vertex conversion
-- ##########################################################################

convertVertex :: Name -> CDGraph -> Vertex CDNode -> String
convertVertex quali g v = case v of
    Vertex (Package name) ->
        let nameStr = format name
            adjas   = adjacents g v
            -- TODO: build package hierachy!
            children = filter (\a -> case a of
                                        Vertex (Class n fs ms) -> True
                                        _ -> False
                              ) adjas
        in  "subgraph cluster" ++ (escape nameStr)
                ++ " {"
                ++ " label = \"" ++ nameStr ++ "\"\n"
                ++ (foldr (++) "" (map (\v -> convertVertex name g v ++ "\n") children)) 
                ++  "}"
    Vertex (Class name fields methods) -> 
        let name' = format name
            nameLabel = format $ unjoin quali name
        in 
            (escape name') ++ " [ label = \"{" 
                           ++ nameLabel
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
    Aggregation _ -> _EDGE_AGGREGATION_CONFIG
    Composition _ -> _EDGE_COMPOSITION_CONFIG

convertArrow :: Direction -> Vertex CDNode -> Vertex CDNode -> String
convertArrow d l r = let l' = escape (extractNodeName l)
                         r' = escape (extractNodeName r)
                     in case d of
                        L2R -> l' ++ " -> " ++ r'
                        R2L -> r' ++ " -> " ++ l'
                        _   -> l' ++ " -> " ++ r'

extractNodeName :: Vertex CDNode -> String
extractNodeName (Vertex n) = format n
