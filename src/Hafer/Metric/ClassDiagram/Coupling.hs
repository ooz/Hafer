{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Hafer.Metric.ClassDiagram.Coupling
( Metric
, evaluate
, coupling
) where

import Hafer.Metric.Metric
import Hafer.Data.ClassDiagram

instance Metric CDGraph Int where
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
