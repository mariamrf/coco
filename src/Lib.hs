module Lib where
import ValueLib
import Text.ParserCombinators.Parsec hiding (spaces)

-- Exposed functions
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match found. " ++ show err
    Right val -> "Value = " ++ show val
 