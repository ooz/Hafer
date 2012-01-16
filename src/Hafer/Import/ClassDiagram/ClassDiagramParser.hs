{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hafer.Import.ClassDiagram.ClassDiagramParser
( ImportMethod 
, imprt
--, classdiagram
) where

import Hafer.Import.ImportMethod

import Hafer.Parser.Parser as P
import Hafer.Data.GenericGraph
import Hafer.Data.ClassDiagram

instance ImportMethod String CDGraph where
    imprt s = P.parse classdiagram s

testClasses = testClass1 ++ " " ++ testClass2 ++ "     " ++ testClass3
testClass1 = "[Foo| -bar : t; baz:s;bob |+bla();~blub(i : int, j:int) : Bob]"
testClass2 = "[Bob |-bar :t; baz :s;foo]"
testClass3 = "[Baz|+boz() : List<Foo<K, V>>]"
testClass4 = "[Importodule|+imprt() : Graph]"

classdiagram :: Parser Char CDGraph
classdiagram = do comps <- many classDiagramComp 
                  return $ MessyGraph comps

classDiagramComp :: Parser Char (GraphElem CDNode CDAssoc)
classDiagramComp = try (do a <- association;
                           return $ GEdge a)
               <|> do c <- clazz;
                      return $ GVertex c -- TODO: add package

clazz :: Parser Char (Vertex CDNode)
clazz = do c <- white $ brackets classBody
           return $ Vertex c []

classBody :: Parser Char CDNode
classBody = try (do className <- name;
                    white $ expect '|';
                    fields <- sepEndBy1 field semi;
                    white $ expect '|';
                    methods <- sepEndBy1 method semi;
                    return $ Class className fields methods)
        <|> try (do className <- name;
                    white $ expect '|';
                    methods <- sepEndBy1 method semi;
                    return $ Class className [] methods)
        <|> try (do className <- name;
                    white $ expect '|';
                    fields <- sepEndBy1 field semi;
                    return $ Class className fields [])
        <|> do className <- name
               return $ Class className [] []

field :: Parser Char Field
field = do v <- white $ visibility;
           n <- name;
           t <- optType;
           return $ Field v n t

optType :: Parser Char Type
optType = do mbType <- optionMaybe (do white $ expect ':';
                                       t <- typ;
                                       return t);
             case mbType of
                Nothing -> return TypeNotSpecified
                Just t  -> return t

typ :: Parser Char Type
typ = try (do tBase   <- name;
              expect  '<';
              tParams <- sepEndBy typ comma;
              expect  '>';
              return $ PolymorphicType tBase tParams)
  <|> do t <- name;
         return $ SimpleType t

visibility :: Parser Char Visibility
visibility = option VisNotSpecified $
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
association :: Parser Char (Edge CDNode CDAssoc)
association = do a            <- clazz;
                 (as, direct) <- assoc;
                 b            <- clazz;
                 return $ Edge as direct a b 

assoc :: Parser Char (CDAssoc, Direction)
assoc = extend

extend :: Parser Char (CDAssoc, Direction)
extend = do expect '^';
            many1 $ expect '-';
            return $ (Extend, R2L)
     <|> do many1 $ expect '-';
            expect '^';
            return $ (Extend, L2R)

-- relation :: Parser Char Edge
-- relation = do a <- clazz;
--               expect '-';
--               many $ expect '-';
--               b <- clazz;
--               return $ Association a b []
