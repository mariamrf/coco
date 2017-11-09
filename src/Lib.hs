module Lib
    ( readExpr
    ) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

-- Primitive/basic types
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float

instance Show LispVal where
    show (Atom name) = name
    show (String contents) = "\"" ++ contents ++ "\""
    show (Number num) = show num
    show (Bool True) = "True"
    show (Bool False) = "False"
    show (Character c) = "'" ++ [c] ++ "'"
    show (Float f) = show f

-- Parsers/parser actions for LispVal
parseChar :: Parser LispVal
parseChar = do
                try $ string "#\\"
                x <- parseCharName <|> anyChar
                return $ Character x

parseCharName :: Parser Char
parseCharName = do -- here "space" and "newline" are lowercase only even tho standard says case insensitive
                    x <- try (string "space" <|> string "newline")
                    case x of
                        "space" -> do return ' '
                        "newline" -> do return '\n'

parseString :: Parser LispVal
parseString = do
            char '"'
            x <- many $ many1 (noneOf "\"\\") <|> escapeChars
            char '"'
            return $ String (concat x)

escapeChars :: Parser String
escapeChars = do
                char '\\'
                x <- oneOf "\\\"ntr"
                case x of
                    '"' -> do return [x]
                    't' -> do return "\t"
                    'n' -> do return "\n"
                    'r' -> do return "\r"

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = [first] ++ rest
                return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = do
                num <- parseDec <|> parseDec2 <|> parseHex <|> parseOct <|> parseBin
                return $ num

parseDec :: Parser LispVal
parseDec = many1 digit >>= return . Number . read

parseDec2 :: Parser LispVal
parseDec2 = do
                try $ string "#d"
                x <- many1 digit
                return $ Number (read x)

parseHex :: Parser LispVal
parseHex = do
                try $ string "#x"
                x <- many1 hexDigit
                let dec = fst $ readHex x !! 0
                return $ Number dec

parseOct :: Parser LispVal
parseOct = do
                try $ string "#o"
                x <- many1 octDigit
                let dec = fst $ readOct x !! 0
                return $ Number dec

parseBin :: Parser LispVal
parseBin = do
                try $ string "#b"
                x <- many1 (oneOf "01")
                return $ Number (bin2dec x)


bin2dec = bin2dec' 0
bin2dec' x "" = x
bin2dec' x (y:ys) = let old = 2 * x + (if y == '0' then 0 else 1) in bin2dec' old ys

parseBool :: Parser LispVal
parseBool = do
                char '#'
                x <- oneOf "tf"
                return $ case x of
                    't' -> Bool True
                    'f' -> Bool False

parseFloat :: Parser LispVal
parseFloat = do
                first <- many1 digit
                char '.'
                last <- many1 digit
                let num = fst $ readFloat (first ++ "." ++ last) !! 0
                return $ Float num
-- todo: add/reshape parsers to represent full numeric tower (R5RS)


parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseFloat
            <|> try parseNumber
            <|> try parseBool
            <|> try parseChar

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match found. " ++ show err
    Right val -> "Value = " ++ show val
