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
eval val@(Float _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val
eval (List [Atom func, arg]) = apply_single func $ eval arg
eval (List (Atom func : args)) = apply func $ map eval args
eval x = String $ show x
