module Hafer.Parser.Parsec 
( Parser, Hafer.Parser.Parsec.parse
, next, reject, eof 
, choice, option
, satisfy, expect, expects
, many, many1, (<|>)
, parens, braces, brackets
, number, name, whitespace, white
, reserved
, optionMaybe
, sepEndBy
, sepEndBy1
, comma, semi
, P.symbol
, try
) where

import Data.Char

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Prim

import Hafer.Parser.Language (diagramDef)

type Parser t = Parsec [t] ()

{-
Mostly copied from:
Compilerbau WS 2011/12
Prof. Waldmann
-}
parse :: Parser Char a -> String -> a
parse p ts = 
  case Text.Parsec.parse (complete p) "<input>" ts of
    Right a -> a
    Left msg -> 
        let p = errorPos msg
            ( pre, this : post ) 
                = splitAt ( sourceLine p - 1 ) $ lines ts
            underline 
                = replicate ( sourceColumn p - 1 ) '-' ++ "^"
        in  error $ unlines [ "parse error", this, underline, show msg ]
    
complete :: Parser Char a -> Parser Char a
complete p = do  whitespace ;
                 x <- p ; 
                 eof ; 
                 return x
    
lexer = P.makeTokenParser diagramDef

-- ( )
parens   = P.parens lexer
-- { }
braces   = P.braces lexer
-- [ ]
brackets = P.brackets lexer

comma = P.comma lexer
semi = P.semi lexer

reserved = P.reserved lexer
name = P.identifier lexer

next :: Parser Char Char
next = anyChar
    
reject :: Parser t e
reject = parserZero

expect :: Char -> Parser Char Char
expect = char
             
expects :: String -> Parser Char ()    
expects s = P.reserved lexer s
                    
number :: Parser Char Integer
number = P.natural lexer
    
whitespace = P.whiteSpace lexer
white p = do x <- p ; 
             whitespace ; 
             return x
