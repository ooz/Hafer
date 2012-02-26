{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hafer.Import.ClassDiagram.ClassDiagramParser
( ImportMethod 
, imprt
) where

import Data.List (partition)

import Hafer.Import.ImportMethod

import Hafer.Parser.Parser as P
import Hafer.Data.GenericGraph
import Hafer.Data.ClassDiagram

instance ImportMethod String CDGraph where
    imprt s = P.parse classdiagram s

__test__   = testClass1 ++ " " ++ testClass2 ++ " " ++ testAssoc 
testClass1 = "[Foo| -bar : t; baz:s;bob |+bla();~blub(i : int, j:int) : Bob]"
testClass2 = "[Importodule |+imprt() : Graph<Foo<K,V>, B>]"
testAssoc  = "[Foo]---[Bar]"

classdiagram :: Parser Char CDGraph             -- v--- no qualification
classdiagram = do comps <- many $ classDiagramComp $ Name ""
                  return $ ElemSetGraph $ concat comps

classDiagramComp :: Name -> Parser Char [GraphElem CDNode CDAssoc]
classDiagramComp quali = try (do comps <- nonPackageComp quali;
                                 return $ comps)
                  <|> do (vertices, edges) <- package;
                         return $ (map (\v -> GVertex v) vertices) ++ (map (\e -> GEdge e) edges)

nonPackageComp :: Name -> Parser Char [GraphElem CDNode CDAssoc]
nonPackageComp quali = try (do comps <- association quali;
                               return $ comps)
                <|> do c <- clazz quali;
                       return $ [GVertex c]


package :: Parser Char ([Vertex CDNode], [Edge CDNode CDAssoc])
package  = do pname <- componentName $ Name "";
              white $ expect ':';
              comps <- many $ nonPackageComp pname
              let (nodes, edges) = partition (\a -> case a of
                                                 GVertex _ -> True
                                                 GEdge _   -> False)
                                             $ concat comps
                  vertices = map (\a -> case a of
                                         GVertex a' -> a')
                                 nodes
                  edges'   = map (\a -> case a of
                                         GEdge a' -> a')
                                 edges
                  package  = Vertex (Package pname)
                  pkgEdges = map (\v -> Edge PkgContain L2R v package)
                                 vertices
              return ([package] ++ vertices
                     , edges'   ++ pkgEdges
                     )

clazz :: Name -> Parser Char (Vertex CDNode)
clazz quali = do c <- white $ brackets $ classBody quali
                 return $ Vertex c 

classBody :: Name -> Parser Char CDNode
classBody quali = try (do className <- componentName quali;
                          white $ expect '|';
                          fields <- sepEndBy1 field semi;
                          white $ expect '|';
                          methods <- sepEndBy1 method semi;
                          return $ Class className fields methods)
              <|> try (do className <- componentName quali;
                          white $ expect '|';
                          methods <- sepEndBy1 method semi;
                          return $ Class className [] methods)
              <|> try (do className <- componentName quali;
                          white $ expect '|';
                          fields <- sepEndBy1 field semi;
                          return $ Class className fields [])
              <|> do className <- componentName quali;
                     return $ Class className [] []

field :: Parser Char Field
field = do v <- white $ visibility;
           n <- name;
           t <- optType;
           return $ Field v n t

-- | Parses a class diagram component name, e.g. name of a class or package.
componentName :: Name -> Parser Char Name
componentName quali = try (do quali' <- name;
                              expect '.';
                              namePart <- componentName $ Name "";
                              return $ join quali $ Qualified quali' namePart)
                  <|> try (do n <- name;
                              expect '<';
                              nParams <- sepEndBy name comma;
                              white $ expect '>';
                              return $ join quali $ Parametrized n nParams)
                  <|> do n <- name;
                         return $ join quali $ Name n

optType :: Parser Char Type
optType = do mbType <- optionMaybe (do white $ expect ':';
                                       t <- typ;
                                       return t);
             case mbType of
                Nothing -> return Dynamic
                Just t  -> return t

typ :: Parser Char Type
typ = try (do tBase   <- name;
              expect  '<';
              tParams <- sepEndBy typ comma;
              expect  '>';
              return $ PolymorphicType tBase tParams)
  <|> do t <- name;
         return $ Type t

visibility :: Parser Char Visibility
visibility = option VisDefault $
                do expect '-';
                   return VisPrivate
            <|> do expect '~';
                   return VisPackage
            <|> do expect '+';
                   return VisPublic

method :: Parser Char Method
method = do v <- visibility;
            n <- name;
            params <- parens $ sepEndBy param comma;
            t <- white $ optType;
            return $ Method v n params t

param :: Parser Char Param
param = do n <- name;
           t <- optType;
           return (n, t)



-- # Association parsers
association :: Name -> Parser Char [GraphElem CDNode CDAssoc]
association quali = do a            <- clazz quali;
                       (as, direct) <- assoc;
                       b            <- clazz quali;
                       return $ [ GVertex a
                                , GVertex b
                                , GEdge $ Edge as direct a b ]

assoc :: Parser Char (CDAssoc, Direction)
assoc = try (extend)
    <|> try (plainAssoc)
    <|> try (composition)
    <|> aggregation

extend :: Parser Char (CDAssoc, Direction)
extend = do expect '^';
            many1 $ expect '-';
            return $ (Extend, R2L)
     <|> do many1 $ expect '-';
            expect '^';
            return $ (Extend, L2R)

plainAssoc :: Parser Char (CDAssoc, Direction)
plainAssoc = try (do expect '<';
                     many1 $ expect '-';
                     return $ (Association [], R2L))
         <|> try (do expect '<';
                     many1 $ expect '-';
                     expect '>';
                     return $ (Association [], Both))
         <|> try (do many1 $ expect '-';
                     expect '>';
                     return $ (Association [], L2R))
         <|>      do many1 $ expect '-';
                             return $ (Association [], None)

composition :: Parser Char (CDAssoc, Direction)
composition = do expects "++";
                 many1 $ expect '-';
                 expect '>';
                 return $ (Composition [], L2R)
          <|> do expect '<';
                 many1 $ expect '-';
                 expects "++";
                 return $ (Composition [], R2L)

aggregation :: Parser Char (CDAssoc, Direction)
aggregation = do choice $ [expects "<>", expects "+"];
                 many1 $ expect '-';
                 expect '>';
                 return $ (Composition [], L2R)
          <|> do expect '<';
                 many1 $ expect '-';
                 choice $ [expects "<>", expects "+"];
                 return $ (Composition [], R2L)
