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
    deriving (Show, Eq)

instance Format CDNode where
    format n = case n of
        Class name _ _   -> format name
        Interface name _ -> format name
        Package name     -> format name

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
          | Qualified String Name
          | Parametrized String [String] 
    deriving (Show, Eq)

instance Format Name where
    format n = case n of
        Name ns -> ns
        Qualified qs n' -> qs ++ "." ++ format n'
        Parametrized ns ps -> ns -- TODO: maybe include parameters in formatted string

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
                   

data Type = Dynamic
          | Type            String
          | QualifiedType   String Type
          | PolymorphicType String [Type]
    deriving (Show, Eq)

data Visibility = VisDefault
                | VisPrivate
                | VisPackage
                | VisPublic
    deriving (Show, Eq)

type Label = String
data AssocProp = LeftEnd  Label Multiplicity
               | RightEnd Label Multiplicity
               | Center Label
    deriving (Show, Eq)
                     
data Multiplicity = Custom String
    deriving (Show, Eq)

