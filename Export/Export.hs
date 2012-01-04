module Export.Export 
( exprt
, ExportType(GraphvizDot)
) where

import Data.DiagramGraph
import qualified Export.ClassDiagram.Dot as CDD

data ExportType = GraphvizDot
                | Foo

exprt :: DiagramGraph -> ExportType -> String
exprt g t = case t of
    GraphvizDot -> CDD.export g
    _ -> error "Unsupported export type!"
     
