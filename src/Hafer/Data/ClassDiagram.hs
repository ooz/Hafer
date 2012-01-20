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
    deriving (Show, Eq)

data CDAssoc = Association [AssocProp]
             | Extend
             | Aggregation [AssocProp]
             | Composition [AssocProp]
    deriving (Show, Eq)

type Label = String
type Name  = String
type Param = (Name, Type)

data Field = Field Visibility Name Type
    deriving (Show, Eq)

data Method = Method Visibility Name [Param] Type
    deriving (Show, Eq)

data Type = TypeNotSpecified
          | SimpleType      String
          | PolymorphicType String [Type]
    deriving (Show, Eq)

data Visibility = VisNotSpecified
                | VisDefault
                | VisPrivate
                | VisPackage
                | VisPublic
    deriving (Show, Eq)

data AssocProp = LeftEnd  Label Multiplicity
               | RightEnd Label Multiplicity
               | Center Label
    deriving (Show, Eq)
                     
data Multiplicity = Custom String
    deriving (Show, Eq)

