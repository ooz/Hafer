{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hafer.Metric.ClassDiagram.Coupling
( evaluate
, coupling
) where

import Hafer.Data.ClassDiagram

evaluate g = coupling g

coupling :: CDGraph -> Int
coupling g = let
                vs  = vertices g
                es' = filter (\(Edge e d a b) -> case e of 
                                                PkgContain -> False
                                                _          -> True) $ edges g
                g'  = MathGraph vs es'
             in
                maxDegree g' 
