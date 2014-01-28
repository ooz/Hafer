{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hafer.Export.ClassDiagram.Java 
( exprt
, export
) where

import Data.List (find)

import Hafer.Data.ClassDiagram

exprt :: CDGraph -> String
exprt g = export g

export :: CDGraph -> String
export g = let 
            vs = vertices g
            es = edges g
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
            foldr (++) "" (map (\cv -> convertVertex (Name "") g cv ++ "\n") $ pkgVerts ++ nonContainedVerts)

replace :: Char -> String -> String -> String
replace c r s = case s of
    ""  -> ""
    h:t -> if (h == c)
           then r ++ (replace c r t)
           else h:(replace c r t)

convertVertex :: Name -> CDGraph -> Vertex CDNode -> String
convertVertex quali g v = case v of
    Vertex (Package name) ->
        let pkgName = format name
            adjas   = adjacents g v
            classes = filter (\a -> case a of
                                        Vertex (Class _ _ _) -> True
                                        _ -> False
                             ) adjas 
        in  (replace '.' "/" pkgName) ++ ":\n"
            ++ (foldr (++) "" (map (\c -> convertVertex name g c ++ "\n") classes))
    Vertex (Class name fields methods) ->
        let packageStr = format quali
            packageLine = if (packageStr /= "")
                            then "package " ++ packageStr ++ ";\n"
                            else ""
            className = replace '\\' "" $ format $ unjoin quali name
            classVis  = "public "
            edgesForClass = edgesFor g v
            superClasses  = filter (\(Edge e d a b) -> case e of
                                                        Extend | (d == L2R && a == v || d == R2L && b == v) -> True
                                                        _ -> False) edgesForClass
            superClass = case (map (\(Edge _ d a b) -> if (a == v) then b else a) superClasses) of
                            []  -> Nothing
                            h:t -> Just h
            extend = case superClass of
                        Nothing -> ""
                        Just (Vertex s)  -> " extends " ++ (replace '\\' "" $ format $ unjoin quali (nodeName s))
        in packageLine 
           ++ "\n"
           ++ classVis ++ "class " ++ className ++ extend ++ " {\n"
           ++ "\n"
           ++ (foldr (++) "" (map (\f -> convertField  "    " f) fields))
           ++ "\n"
           ++ (foldr (++) "" (map (\m -> convertMethod "    " m ++ "\n") methods))
           ++ "}\n"
    _ -> ""

convertField :: String -> Field -> String
convertField p (Field v n t) = 
                   let
                    fVis  = convertVisibility v
                    fType = convertOptType t
                   in
                    p ++ fVis ++ fType ++ n ++ ";"

convertMethod :: String -> Method -> String
convertMethod p (Method v n ps t) = 
                   let
                    mVis  = convertVisibility v
                    mType = convertOptTypeMethod t
                   in
                    p ++ mVis ++ mType ++ n ++ (convertParams ps) ++ " {\n" ++
                    p ++ "}\n"

convertVisibility :: Visibility -> String
convertVisibility v = case v of
    VisPrivate -> "private "
    VisPackage -> ""
    VisPublic  -> "public "
    _          -> "protected "

convertOptType :: Type -> String
convertOptType t = case t of
    Dynamic -> "Object "
    _ -> convertType t ++ " "

convertOptTypeMethod :: Type -> String
convertOptTypeMethod t = case t of
    Dynamic -> "void "
    _ -> convertType t ++ " "

convertType :: Type -> String
convertType t = format t

convertTypes :: [Type] -> String
convertTypes ts = case (map (\a -> convertType a) ts) of
    t:ts' -> reduceSep (t:ts') ", "
    []    -> ""

convertParams :: [Param] -> String
convertParams l = case (map (\a -> convertParam a) l) of
    p:ps -> "(" ++ (reduceSep (p:ps) ", ") ++ ")"
    []   -> "()"

convertParam :: Param -> String
convertParam (n,t) = (convertOptType t) ++ n

