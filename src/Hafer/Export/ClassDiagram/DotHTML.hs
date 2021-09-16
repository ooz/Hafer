{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hafer.Export.ClassDiagram.DotHTML
--( ExportMethod
--, exprt
( exprt
, export
) where

import Data.List (find, isInfixOf)
import Data.String.Utils (replace)

import Hafer.Export.Common.Dot ( cGRAPH_START
                               , cGRAPH_END
                               , cGENERAL_CONFIG
                               , reservedWords
                               , escape
                               , escapeChar
                               , escapeReserved
                               , escapeHTML
                               )
-- import Hafer.Export.ExportMethod
import Hafer.Data.ClassDiagram

-- instance ExportMethod CDGraph String where
--     exprt g = export g



-- ##########################################################################
-- # Additional boilerplate
-- ##########################################################################

_SPLINE_CONFIG = "splines = \"polyline\";" -- \"ortho\"

_NODE_CONFIG = "node [\
\ fontname = \"Bitstream Vera Sans\"\
\ fontsize = 8\
\ shape = \"none\"\
\ ]"

_EDGE_CONFIG = "edge [\
\ fontname = \"Bitstream Vera Sans\"\
\ fontsize = 8\
\ labeldistance = 1.5\
\ ]"

_EDGE_EXTEND_CONFIG = "edge [ arrowhead = \"empty\" arrowtail = \"none\" taillabel=\"\" headlabel=\"\" label=\"\" ]"
_EDGE_ASSOCIATION_CONFIG_START = "edge [ arrowhead = \"none\" arrowtail = \"none\" dir=both"
_EDGE_AGGREGATION_CONFIG_START = "edge [ arrowhead = \"vee\" arrowtail = \"odiamond\" dir=both"
_EDGE_COMPOSITION_CONFIG_START = "edge [ arrowhead = \"vee\" arrowtail = \"diamond\" dir=both"
_EDGE_CONFIG_END = " ]"



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
                ++ _SPLINE_CONFIG ++ "\n"
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
            nameLabel     = format $ unjoin quali name
            nameLabelHTML = replace "\\<" "&lt;" $ replace "\\>" "&gt;" nameLabel
        in 
            (escape name') ++ " [ label = <<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">" 
                           ++ "<TR><TD PORT=\"p\">" ++ nameLabelHTML ++ "</TD></TR>"
                           ++ (addFields fields) 
                           ++ (addMethods methods) 
                           ++ "</TABLE>> ]"  
    _ -> ""

addFields :: [Field] -> String
addFields l = case l of 
    a:as -> "<TR><TD ALIGN=\"LEFT\" BALIGN=\"LEFT\">" ++ (convertFields l) ++ "</TD></TR>"
    []   -> ""
convertFields :: [Field] -> String
convertFields fields = case fields of
    (Field v name t):fs -> (convertVisibility v) 
                           ++ name 
                           ++ (convertOptType t) 
                           ++ "<BR/>" 
                           ++ convertFields fs
    []   -> ""

addMethods :: [Method] -> String
addMethods l = case l of 
    a:as -> "<TR><TD ALIGN=\"LEFT\" BALIGN=\"LEFT\">" ++ (convertMethods l) ++ "</TD></TR>"
    []   -> ""
convertMethods :: [Method] -> String
convertMethods methods = case methods of
    (Method v name params t):ms -> (convertVisibility v)
                                   ++ name 
                                   ++ "(" ++ (convertParams params) ++ ")"
                                   ++ (convertOptType t)
                                   ++ "<BR/>"
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
convertType t = escapeHTML $ format t

convertTypes :: [Type] -> String
convertTypes ts = case (map (\a -> convertType a) ts) of
    t:ts' -> reduceSep (t:ts') ", "
    []    -> ""

convertParams :: [Param] -> String
convertParams l = case (map (\a -> convertParam a) l) of
    p:ps -> reduceSep (p:ps) ", "
    []   -> ""

convertParam :: Param -> String
convertParam (n,t) = n ++ convertOptType t



-- ##########################################################################
-- # Edge conversion
-- ##########################################################################

convertEdge :: Edge CDNode CDAssoc -> String
convertEdge e = case e of
    Edge assoc dir l r -> (convertEdgeType assoc dir) ++ "\n"
                          ++ (convertArrow dir l r)

convertEdgeType :: CDAssoc -> Direction -> String
convertEdgeType a d = case a of
    Extend -> _EDGE_EXTEND_CONFIG
    Association props -> _EDGE_ASSOCIATION_CONFIG_START ++ addMissingProps (convertAssocProps d props) ++ _EDGE_CONFIG_END
    Aggregation props -> _EDGE_AGGREGATION_CONFIG_START ++ addMissingProps (convertAssocProps d props) ++ _EDGE_CONFIG_END 
    Composition props -> _EDGE_COMPOSITION_CONFIG_START ++ addMissingProps (convertAssocProps d props) ++ _EDGE_CONFIG_END

convertArrow :: Direction -> Vertex CDNode -> Vertex CDNode -> String
convertArrow d l r = let l' = (escape (extractNodeName l)) ++ ":p"
                         r' = (escape (extractNodeName r)) ++ ":p"
                     in case d of
                        L2R -> l' ++ " -> " ++ r'
                        R2L -> r' ++ " -> " ++ l'
                        _   -> l' ++ " -> " ++ r'

addMissingProps :: String -> String
addMissingProps str = case (isInfixOf "taillabel" str) of
    False -> addMissingProps $ str ++ " taillabel = \"\""
    True  -> case (isInfixOf "headlabel" str) of
              False -> addMissingProps $ str ++ " headlabel = \"\""
              True  -> case (isInfixOf " label " str) of
                        False -> addMissingProps $ str ++ " label = \"\""
                        True  -> str

convertAssocProps :: Direction -> [AssocProp] -> String
convertAssocProps d ps = case ps of
    p:ps' -> convertAssocProp d p ++ convertAssocProps d ps'
    []    -> ""


convertAssocProp :: Direction -> AssocProp -> String
convertAssocProp d@(L2R) p@(LeftEnd  label card) = " taillabel = \"" ++ convertLabelCard label card ++"\""
convertAssocProp d@(R2L) p@(LeftEnd  label card) = " headlabel = \"" ++ convertLabelCard label card ++"\""
convertAssocProp d@(_)   p@(LeftEnd  label card) = " taillabel = \"" ++ convertLabelCard label card ++"\""
convertAssocProp d@(L2R) p@(RightEnd label card) = " headlabel = \"" ++ convertLabelCard label card ++"\""
convertAssocProp d@(R2L) p@(RightEnd label card) = " taillabel = \"" ++ convertLabelCard label card ++"\""
convertAssocProp d@(_)   p@(RightEnd label card) = " headlabel = \"" ++ convertLabelCard label card ++"\""
convertAssocProp d@(_)   p@(Center   label)      = " label = \"" ++ label ++"\""

convertLabelCard :: String -> Cardinality -> String
convertLabelCard l c = case l of
    "" -> convertCardinality c
    _  -> case convertCardinality c of 
            ""  -> l
            str -> str ++ " " ++ l

convertCardinality :: Cardinality -> String
convertCardinality c = case c of
    CustomCard str -> str
    ZeroOneCard    -> "0..1"
    ZeroManyCard   -> "0..*"
    OneCard        -> "1"
    OneManyCard    -> "1..*" 

extractNodeName :: Vertex CDNode -> String
extractNodeName (Vertex n) = format n
