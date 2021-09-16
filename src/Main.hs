module Main
( main
) where

import System.Environment (getArgs)

import Data.Char
import Data.List (isInfixOf)
import Data.Data
import Data.Maybe

import Hafer.Data.ClassDiagram
import qualified Hafer.Import.ClassDiagram.ClassDiagramParser as I

import qualified Hafer.Export.ClassDiagram.DotHTML as CD2Dot
import qualified Hafer.Export.ClassDiagram.Java as CD2Java
import qualified Hafer.Export.Graph.Dot as G2Dot

import Hafer.Metric.ClassDiagram.Coupling as CDMCoup

main :: IO ()
main = do [options, inFile] <- getArgs;
          input  <- readFile inFile;
          case input of
              _ | (isInfixOf "--graph" options)    -> putStr $ G2Dot.exprt ((I.imprt input) :: CDGraph)
              _ | (isInfixOf "--classdiagram"   options)    -> putStr $ CD2Dot.exprt ((I.imprt input) :: CDGraph)
              _ | (isInfixOf "--java"   options)    -> putStr $ CD2Java.exprt ((I.imprt input) :: CDGraph)
              _ | (isInfixOf "--coupling" options) -> putStr $ (show ((CDMCoup.evaluate ((I.imprt input) :: CDGraph)) :: Int)) ++ "\n"

