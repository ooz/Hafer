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

data Field = Field Visibility String Type
    deriving (Show, Eq)

data Method = Method Visibility String [Param] Type
    deriving (Show, Eq)

type Param = (String, Type)

-- Name and Type too similar
data Name = Name String
          | QualifiedName String Name
          | ParametrizedName String [String] 
    deriving (Show, Eq)

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

