module Main
( main
) where

import System (getArgs)

import Data.Char
import Data.Data
import Data.Maybe

import Hafer.Data.ClassDiagram
import qualified Hafer.Import.ClassDiagram.ClassDiagramParser as I
import qualified Hafer.Export.ClassDiagram.Dot as E

main :: IO ()
main = do [inFile] <- getArgs;
          input  <- readFile inFile;
          putStr $ E.exprt ((I.imprt input) :: CDGraph)

-- parse :: String -> Graph
-- parse s = Node "foobar" 

-- TODO: define correctly
-- type ImportMethod = String
-- transform :: Graph -> ImportMethod -> String
-- transform g m = ""

testClass = "[Class1]"
testRel   = "[Class1]---[Class2]"
