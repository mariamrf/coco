module ValueLib where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

-- Primitive/basic types
data Value = Atom String
             | List [Value]
             | DottedList [Value] Value
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float

instance Show Value where
    show (Atom name) = name ++ " (atom)"
    show (String contents) = "\"" ++ contents ++ "\"" ++ " (string)"
    show (Number num) = show num ++ " (number)"
    show (Bool True) = "#t (boolean)"
    show (Bool False) = "#f (boolean)"
    show (Character c) = "'" ++ [c] ++ "'"
    show (Float f) = show f ++ " (float)"
    show (List ls) = show ls ++ " (list)"
    show (DottedList xs x) = show xs ++ " . " ++ show x ++ " (dotted list)" 

parseExpr :: Parser Value
parseExpr = try parseFloat
            <|> try parseNumber
            <|> try parseAtom
            <|> parseString
            <|> try parseBool
            <|> try parseChar
            <|> parseQuoted
            <|> parseGenList

-- Parsers/parser actions for Value
parseChar :: Parser Value
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

parseString :: Parser Value
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

parseAtom :: Parser Value
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = [first] ++ rest
                return $ Atom atom

parseNumber :: Parser Value
parseNumber = do
                num <- parseNeg <|> parseDec <|> parseDec2 <|> parseHex <|> parseOct <|> parseBin
                return $ num

parseDec :: Parser Value
parseDec = many1 digit >>= return . Number . read

parseNeg :: Parser Value
parseNeg = do
            try $ char '-'
            x <- many1 digit
            let num = (read x) * (-1)
            return $ Number num

parseDec2 :: Parser Value
parseDec2 = do
                try $ string "#d"
                x <- many1 digit
                return $ Number (read x)

parseHex :: Parser Value
parseHex = do
                try $ string "#x"
                x <- many1 hexDigit
                let dec = fst $ readHex x !! 0
                return $ Number dec

parseOct :: Parser Value
parseOct = do
                try $ string "#o"
                x <- many1 octDigit
                let dec = fst $ readOct x !! 0
                return $ Number dec

parseBin :: Parser Value
parseBin = do
                try $ string "#b"
                x <- many1 (oneOf "01")
                return $ Number (bin2dec x)


bin2dec = bin2dec' 0
bin2dec' x "" = x
bin2dec' x (y:ys) = let old = 2 * x + (if y == '0' then 0 else 1) in bin2dec' old ys

parseBool :: Parser Value
parseBool = do
                char '#'
                x <- oneOf "tf"
                return $ case x of
                    't' -> Bool True
                    'f' -> Bool False

parseFloat :: Parser Value
parseFloat = do
                num <- parseFloatPos <|> parseFloatNeg
                return $ num

parseFloatPos :: Parser Value
parseFloatPos = do
                first <- many1 digit
                char '.'
                last <- many1 digit
                let num = fst $ readFloat (first ++ "." ++ last) !! 0
                return $ Float num

parseFloatNeg = do
                char '-'
                first <- many1 digit
                char '.'
                last <- many1 digit
                let num = (-1) * (fst $ readFloat (first ++ "." ++ last) !! 0)
                return $ Float num
-- todo: add/reshape parsers to represent full numeric tower (R5RS)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseGenList :: Parser Value
parseGenList = do
                char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x

parseList :: Parser Value
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser Value
parseDottedList = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList head tail

parseQuoted :: Parser Value
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]