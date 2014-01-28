module Hafer.Data.ClassDiagram 
( module Hafer.Data.ClassDiagram
, module Hafer.Data.GenericGraph
) where
-- ( Name
-- , Type
-- , Param
-- , Label
-- , Field(Field)
-- , Method(Method)
-- , Visibility(NotSpecified,Default,Private,Package,Public)
-- ) where

import Hafer.Data.GenericGraph

type CDGraph = Graph CDNode CDAssoc

data CDNode = Class Name [Field] [Method]
            | Interface Name [Method]
            | Package Name
            | Reference Name
    deriving (Show)

instance Eq CDNode where
    (==) a b | nodeName a == nodeName b = case (a, b) of
        (Reference _, _)                   -> True
        (_, Reference _)                   -> True
        (Class _ afs ams, Class _ bfs bms) -> afs == bfs && ams == bms
        (Interface _ ams, Interface _ bms) -> ams == bms
        (Package _, Package _)             -> True
        (_, _)                             -> False
    (==) a b | nodeName a /= nodeName b = False

nodeName :: CDNode -> Name
nodeName node = case node of
    Class n _ _   -> n
    Interface n _ -> n
    Package n     -> n
    Reference n   -> n

instance Format CDNode where
    format n = case n of
        Class name _ _   -> format name
        Interface name _ -> format name
        Package name     -> format name
        Reference name   -> format name

data CDAssoc = Association [AssocProp]
             | Extend
             | Aggregation [AssocProp]
             | Composition [AssocProp]
             | PkgContain
    deriving (Show, Eq)

instance Format CDAssoc where
    format e = case e of
        Association _ -> "Association"
        Extend        -> "Extend"
        Aggregation _ -> "Aggregation"
        Composition _ -> "Composition"
        PkgContain    -> "Package"

data Field = Field Visibility String Type
    deriving (Show, Eq)

data Method = Method Visibility String [Param] Type
    deriving (Show, Eq)

type Param = (String, Type)

-- Name and Type too similar
data Name = Name String
          | Parametrized String [Name] 
          | Qualified String Name
--                    ^---------- Qualification/namespace
--                           ^--- Name that is qualified
    deriving (Show, Eq)

instance Format Name where
    format n = case n of
        Name ns -> ns
        Qualified qs n' -> qs ++ "." ++ format n'
        Parametrized ns ps -> ns ++ "\\<" ++ (reduceSep (map format ps) ", ") ++ "\\>" -- TODO: maybe include parameters in formatted string

instance Ord Name where
    (<=) a b = isPrefix a b

reduceSep :: [String] -> String -> String
reduceSep (l:ls) s = foldl (\a b -> a ++ s ++ b) l ls

-- | Finds the longest common prefix of two names.
commonName :: Name -> Name -> Maybe Name
commonName a b = case a of
    Name an -> case b of
        Name bn           -> if (an == bn)
                             then Just $ Name an
                             else Nothing
        Parametrized bn _ -> if (an == bn)
                             then Just $ Name an
                             else Nothing
        Qualified _ _     -> commonName b a
    Qualified qa a' -> case b of
        Name bn -> if (qa == bn)
                   then Just $ Name qa
                   else Nothing
        Parametrized bn _ -> if (qa == bn)
                             then Just $ Name qa
                             else Nothing
        Qualified qb b' -> if (qa == qb) 
                           then case (commonName a' b') of
                            Nothing -> Just $ Name qa
                            Just n  -> Just $ Qualified qa n
                           else Nothing
    _ -> commonName b a

-- | Joins two names where the first name is the qualification for the 
--   second name.
--   (Name "") or (Parametrized "" _) 
--   can be used to indicate no qualification                
join :: Name -> Name -> Name
join a b = case a of
    Name ""           -> b
    Name a'           -> Qualified a' b
    Parametrized a' _ -> Qualified a' b
    Qualified qn a'   -> Qualified qn $ join a' b

isPrefix :: Name -> Name -> Bool
isPrefix q n = case (commonName q n) of
    Nothing -> False
    Just q' -> if (q' == q)
               then True
               else False

-- | Removes the qualification (prefix) q from the name n
--   If q is not a prefix of n, n is returned.
--   If q and n are equal an empty name (Name "") is returned.
--   In case q and n are qualified names of the same length,
--   n without the qualification will be returned.
--
--   Examples:
--    unjoin bob     foo.Bar = foo.Bar
--    unjoin foo     foo.Bar = Bar
--    unjoin foo.Baz foo.Bar = foo.Bar
--    unjoin foo.Bar foo.Bar = ""
unjoin :: Name -> Name -> Name
unjoin q n = case (isPrefix q n) of
    False -> n
    True  -> case q of
              Name qs            -> case n of 
                                    Name _           -> Name ""
                                    Parametrized _ _ -> Name ""
                                    Qualified ns n'  -> n'
              Parametrized qs _  -> unjoin (Name qs) n
              Qualified    qs q' -> case n of
                                    Qualified ns n' -> unjoin q' n'
                                    _               -> n

data Type = Dynamic
          | Type            String
          | PolymorphicType String [Type]
          | QualifiedType   String Type
    deriving (Show, Eq)

instance Format Type where
    format n = case n of
        Type t -> t
        QualifiedType q t' -> q ++ "." ++ format t'
        PolymorphicType t ts -> t ++ "<" ++ (reduceSep (map format ts) ", ") ++ ">" -- TODO: maybe include parameters in formatted string

-- | Converts a name to a type equivalent.
name2type :: Name -> Type
name2type name = case name of 
    Name n -> Type n
    Parametrized n params -> PolymorphicType n (map name2type params)
    Qualified quali name' -> QualifiedType quali $ name2type name'

data Visibility = VisDefault
                | VisPrivate
                | VisPackage
                | VisPublic
    deriving (Show, Eq)

type Label = String
data AssocProp = LeftEnd  Label Cardinality
               | RightEnd Label Cardinality
               | Center   Label
    deriving (Show, Eq)
                     
data Cardinality = CustomCard String
                 | ZeroOneCard
                 | ZeroManyCard
                 | OneCard
                 | OneManyCard
    deriving (Show, Eq)

