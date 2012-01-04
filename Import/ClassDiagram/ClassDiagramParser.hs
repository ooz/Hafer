module Import.ClassDiagram.ClassDiagramParser
( Import.ClassDiagram.ClassDiagramParser.parse 
--, classdiagram
) where

import Parser.Parser
import Data.DiagramGraph

parse :: String -> DiagramGraph
parse input = Parser.Parser.parse classdiagram input

testClasses = testClass1 ++ " " ++ testClass2 ++ "     " ++ testClass3
testClass1 = "[Foo| -bar : t; baz:s;bob |+bla();~blub(i : int, j:int) : Bob]"
testClass2 = "[Bob |-bar :t; baz :s;foo]"
testClass3 = "[Baz|+boz() : Foo]"
testClass4 = "[Importodule|+imprt() : Graph]"


classdiagram :: Parser Char DiagramGraph
classdiagram = do classes <- many clazz
                  return $ DiagramGraph classes []

clazz :: Parser Char Node
clazz = do c <- white $ brackets classBody
           return c

classBody :: Parser Char Node
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
           t <- typ;
           return $ Field v n t

typ :: Parser Char Type
typ = do mbType <- optionMaybe (do white $ expect ':';
                                   t <- name;
                                   return t);
         case mbType of
            Nothing -> return ""
            Just t  -> return t

visibility :: Parser Char Visibility
visibility = option NotSpecified $
             do expect '-';
                return Private
         <|> do expect '~';
                return Package
         <|> do expect '+';
                return Public

method :: Parser Char Method
method = do v <- visibility;
            n <- name;
            params <- parens $ sepEndBy param comma;
            t <- typ;
            return $ Method v n params t

param :: Parser Char Param
param = do n <- name;
           t <- typ;
           return (n, t)


relation :: Parser Char Edge
relation = do a <- clazz;
              expect '-';
              many $ expect '-';
              b <- clazz;
              return $ Association a b []
