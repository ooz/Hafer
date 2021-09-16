module Hafer.Import.Import 
( imprt
, ImportType (ClassDiagram)
) where

import Hafer.Data.DiagramGraph
import qualified Hafer.Import.ClassDiagram.ClassDiagramParser as CDP

data ImportType = ClassDiagram
                 | ERDiagram

imprt :: String -> ImportType -> DiagramGraph
imprt input dia = case dia of
    ClassDiagram -> CDP.parse input 
    _ -> error "Tried to import unsupported diagram type!"
