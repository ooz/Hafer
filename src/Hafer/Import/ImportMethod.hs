{-# LANGUAGE MultiParamTypeClasses #-}
module Hafer.Import.ImportMethod
( module Hafer.Import.ImportMethod
) where

class ImportMethod i g where
    
    imprt :: i -> g

