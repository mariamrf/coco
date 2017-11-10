module Lib where
import LispValLib
import Text.ParserCombinators.Parsec hiding (spaces)

parseExpr :: Parser LispVal
parseExpr = try parseFloat
            <|> try parseNumber
            <|> try parseAtom
            <|> parseString
            <|> try parseBool
            <|> try parseChar

-- Exposed functions
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match found. " ++ show err
    Right val -> "Value = " ++ show val
 