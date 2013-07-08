{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hafer.Import.ClassDiagram.ClassDiagramParser
( imprt
) where

import Data.List (partition, delete)

import Hafer.Parser.Parser as P
import Hafer.Data.GenericGraph
import Hafer.Data.ClassDiagram

imprt s = P.parse classdiagram s

__test__   = testClass1 ++ " " ++ testClass2 ++ " " ++ testAssoc 
testClass1 = "[Foo| -bar : t; baz:s;bob |+bla();~blub(i : int, j:int) : Bob]"
testClass2 = "[Importodule |+imprt() : Graph<Foo<K,V>, B>]"
testAssoc  = "[Foo]---[Bar]"

classdiagram :: Parser Char CDGraph             -- v--- no qualification
classdiagram = do comps <- many $ classDiagramComp $ Name ""
                  return $ removeRedefinitions $ ElemSetGraph $ concat comps
                  return $ ElemSetGraph $ concat comps

-- | Remove duplicate class/interface definitions.
--   Prefers definitions withs fields/methods over definitions
--   with just a name.
--   If both definitions got fields/methods it prefers the first
--   definition (see "greater" function).
removeRedefinitions :: CDGraph -> CDGraph
removeRedefinitions g = 
    let
        vs = vertices g
        es = edges g
        cdNodes = map (\v -> case v of Vertex n -> n) vs
        uniqueNodes = removeNodeDuplicates cdNodes
        vs' = map (\n -> Vertex n) uniqueNodes
    in
        MathGraph vs' es

removeNodeDuplicates :: [CDNode] -> [CDNode]
removeNodeDuplicates ns = case ns of
    []      -> []
    (a:ns') -> let
                (a', ns'') = (findGreatestDefinition a ns')
               in
                a':(removeNodeDuplicates ns'')

findGreatestDefinition :: CDNode -> [CDNode] -> (CDNode, [CDNode])
findGreatestDefinition a ns = case ns of
    []      -> (a, [])
    (b:ns') -> if (nodeName a == nodeName b)
               then findGreatestDefinition (greater a b) ns'
               else let
                     (g, rest) = findGreatestDefinition a ns'
                    in
                     (g, b:rest)

greater :: CDNode -> CDNode -> CDNode 
greater a@(Class an [] [])   b@(Class bn bfs bms) = b
greater a@(Class an afs ams) b@(Class bn [] [])   = a
greater a@(Interface an [])  b@(Interface bn bms) = b
greater a@(Interface an ams) b@(Interface bn [])  = a 
greater a@(Reference _)      b@(_)                = b
greater a@(_)                b@(Reference _)      = a
greater a@(_) b@(_) = a

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
                                         GVertex (Vertex a') -> a')
                                 nodes
                  vertices' = map (\n -> Vertex n) $
                              removeNodeDuplicates vertices
                  edges'   = map (\a -> case a of
                                         GEdge a' -> a')
                                 edges
                  package  = Vertex (Package pname)
                  pkgEdges = map (\v -> Edge PkgContain L2R v package)
                                 vertices'
              return ([package] ++ vertices'
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
              white $ expect '>';
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
association quali = do a@(Vertex (Class aName _ _)) <- clazz quali;
                       (as, direct) <- assoc;
                       b@(Vertex (Class bName _ _)) <- clazz quali;
                       return $ [ GVertex a
                                , GVertex b
                                , GEdge $ Edge as 
                                               direct 
                                               (Vertex $ Reference aName) 
                                               (Vertex $ Reference bName) ]

assoc :: Parser Char (CDAssoc, Direction)
assoc = try (extend)
    <|> cardLabeledAssoc

cardLabeledAssoc :: Parser Char (CDAssoc, Direction)
cardLabeledAssoc = try (plainAssoc)
               <|> try (composition)
               <|> aggregation

extend :: Parser Char (CDAssoc, Direction)
extend = do expect '^';
            many1 $ expect '-';
            return $ (Extend, R2L)
     <|> do many1 $ expect '-';
            expect '^';
            return $ (Extend, L2R)

cardinality :: Parser Char Cardinality
cardinality = try (do expect '1';
                      return $ OneCard)
          <|> do expects "1..*";
                 return $ OneManyCard
          <|> try (do expects "0..1";
                      return $ ZeroOneCard)
          <|> do expects "0..*";
                 return $ ZeroManyCard

role :: Parser Char Label
role = name

leftAssocProp :: Parser Char AssocProp
leftAssocProp = try (do card <- cardinality;
                        return $ LeftEnd "" card)
            <|> do card <- cardinality;
                   labl <- role;
                   return $ LeftEnd labl card
            <|> try (do labl <- role;
                        return $ LeftEnd labl (CustomCard ""))
            <|> do labl <- role;
                   card <- cardinality;
                   return $ LeftEnd labl card

rightAssocProp :: Parser Char AssocProp
rightAssocProp = do prop <- leftAssocProp;
                    case prop of
                        LeftEnd labl card -> return $ RightEnd labl card
                        _                 -> return $ prop

centerAssocProp :: Parser Char AssocProp
centerAssocProp = do labl <- role;
                     return $ Center labl

extractAssocProp :: Maybe AssocProp -> [AssocProp]
extractAssocProp mbAP = case mbAP of
    Just ap -> [ap]
    Nothing -> []
-- short hand:
eAP = extractAssocProp

extractAssocProps :: [Maybe AssocProp] -> [AssocProp]
extractAssocProps mbAPs = foldl (++) [] (map eAP mbAPs)
-- short hand:
eAPs = extractAssocProps

plainAssoc :: Parser Char (CDAssoc, Direction)
plainAssoc = do mbL <- optionMaybe leftAssocProp;
                (Association _, dir) <- plainAssocWithoutProp
                mbR <- optionMaybe rightAssocProp;
                return $ (Association (eAPs [mbL, mbR]), dir)

plainAssocWithoutProp :: Parser Char (CDAssoc, Direction)
plainAssocWithoutProp = try (do expect '<';
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
                 mbR <- optionMaybe rightAssocProp;
                 return $ (Composition (eAP mbR), L2R)
          <|> do mbL <- optionMaybe leftAssocProp;
                 expect '<';
                 many1 $ expect '-';
                 expects "++";
                 return $ (Composition (eAP mbL), R2L)

aggregation :: Parser Char (CDAssoc, Direction)
aggregation = do mbL <- optionMaybe leftAssocProp
                 choice $ [expects "<>", expects "+"];
                 many1 $ expect '-';
                 expect '>';
                 mbR <- optionMaybe rightAssocProp
                 return $ (Aggregation (eAPs [mbL, mbR]), L2R)
          <|> do mbL <- optionMaybe leftAssocProp
                 expect '<';
                 many1 $ expect '-';
                 choice $ [expects "<>", expects "+"];
                 mbR <- optionMaybe rightAssocProp
                 return $ (Aggregation (eAPs [mbL, mbR]), R2L)
