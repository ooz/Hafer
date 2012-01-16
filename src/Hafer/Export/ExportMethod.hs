{-# LANGUAGE MultiParamTypeClasses #-}
module Hafer.Export.ExportMethod
( module Hafer.Export.ExportMethod
) where

class ExportMethod g o where

    exprt :: g -> o

