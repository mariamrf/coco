module Lib where
import ValueLib
import Eval
import Text.ParserCombinators.Parsec hiding (spaces)

-- Exposed functions
readExpr :: String -> Value
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match found. " ++ show err
    Right val -> val

eval :: Value -> Value
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
