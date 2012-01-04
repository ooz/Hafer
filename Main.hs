module Main
( main
) where

import System (getArgs)

import Data.Char
import Data.Data
import Data.Maybe

import Data.DiagramGraph
import Import.Import 
import Export.Export

main :: IO ()
main = do [inFile] <- getArgs;
          input  <- readFile inFile;
          putStr $ exprt (imprt input ClassDiagram) GraphvizDot;

-- parse :: String -> Graph
-- parse s = Node "foobar" 

-- TODO: define correctly
-- type ImportMethod = String
-- transform :: Graph -> ImportMethod -> String
-- transform g m = ""

testClass = "[Class1]"
testRel   = "[Class1]---[Class2]"
