module Hafer.Data.ClassDiagram 
(module Hafer.Data.ClassDiagram) where
-- ( Name
-- , Type
-- , Param
-- , Label
-- , Field(Field)
-- , Method(Method)
-- , Visibility(NotSpecified,Default,Private,Package,Public)
-- ) where

type Label = String
type Name = String
type Type = String
type Param = (Name, Type)

data Field = Field Visibility Name Type
    deriving (Show, Eq)
data Method = Method Visibility Name [Param] Type
    deriving (Show, Eq)

data Visibility = NotSpecified
                | Default
                | Private
                | Package
                | Public
    deriving (Show, Eq)

data AssociationProp = LeftEnd  Label Arrow Multiplicity
                     | RightEnd Label Arrow Multiplicity
                     | Center Label
    deriving (Show, Eq)
                     
data Arrow = None
           | Direct
           | Extend
    deriving (Show, Eq)

data Multiplicity = Custom String
    deriving (Show, Eq)

