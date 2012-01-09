module Hafer.Export.Export 
( exprt
, ExportType(GraphvizDot)
) where

import Hafer.Data.DiagramGraph
import qualified Hafer.Export.ClassDiagram.Dot as CDD

data ExportType = GraphvizDot
                | Foo

exprt :: DiagramGraph -> ExportType -> String
exprt g t = case t of
    GraphvizDot -> CDD.export g
    _ -> error "Unsupported export type!"
     
