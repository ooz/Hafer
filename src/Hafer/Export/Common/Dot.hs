module Hafer.Export.Common.Dot
( module Hafer.Export.Common.Dot
) where

-- ##########################################################################
-- # Constants
-- ##########################################################################

cGRAPH_START = "digraph G {"
cGRAPH_END   = "}"

cGENERAL_CONFIG = "fontname = \"Bitstream Vera Sans\";\
\ rankdir = \"RL\";\
\ fontsize = 8;"



-- ##########################################################################
-- # Escape functions
-- ##########################################################################

reservedWords = ["Graph", "Node", "Edge"]

escape :: String -> String
escape name = escapeReserved $ map escapeChar name 

escapeChar :: Char -> Char
escapeChar c = case c of
    ' ' -> '_'
    '<' -> '_'
    '>' -> '_'
    ',' -> '_'
    '.' -> '_'
    '\\'-> '_'
    _   -> c

escapeReserved :: String -> String
escapeReserved word = case (elem word reservedWords) of
    True  -> "_" ++ word
    False -> word

escapeHTML :: String -> String
escapeHTML word = concat $ map escapeHTMLChar word

escapeHTMLChar :: Char -> String
escapeHTMLChar c = case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    _   -> [c]
