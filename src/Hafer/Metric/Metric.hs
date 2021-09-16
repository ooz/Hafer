{-# LANGUAGE MultiParamTypeClasses #-}
module Hafer.Metric.Metric
( module Hafer.Metric.Metric
) where

class Metric g m where

    evaluate :: g -> m

