module Eval where
import ValueLib

apply :: String -> [Value] -> Value
apply func args = maybe (String $ func ++ " is not a function or expects one arg!") ($ args) $ lookup func primitives

apply_single :: String -> Value -> Value
apply_single func args = maybe (String $ func ++ " is not a function!") ($ args) $ lookup func primitives_single

primitives :: [(String, [Value] -> Value)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp (/)),
              ("mod", intBinOp (mod)),
              ("quotient", intBinOp (quot)),
              ("remainder", intBinOp (rem))]

primitives_single :: [(String, Value -> Value)]
primitives_single = [("number?", numTest),
                     ("string?", strTest),
                     ("boolean?", boolTest),
                     ("list?", listTest),
                     ("zero?", zeroTest),
                     ("char?", charTest),
                     ("symbol?", symTest),
                     ("not", valNot),
                     ("length", valLength),
                     ("symbol->string", symStr True),
                     ("string->symbol", symStr False)]

intBinOp :: (Integer -> Integer -> Integer) -> [Value] -> Value
intBinOp op params = Number $ foldl1 op $ map unpackInt params

numericBinOp :: (Float -> Float -> Float) -> [Value] -> Value
numericBinOp op params = Float $ foldl1 op $ map unpackFloat params

unpackInt :: Value -> Integer
unpackInt (Number n) = n
unpackInt _ = 0 -- for now

unpackFloat :: Value -> Float
unpackFloat (Float n) = n
unpackFloat (Number n) = fromIntegral n
unpackFloat _ = 0 -- for now

numTest :: Value -> Value
numTest (Number _) = Bool True
numTest (Float _) = Bool True
numTest _ = Bool False

strTest :: Value -> Value
strTest (String _) = Bool True
strTest _ = Bool False

boolTest :: Value -> Value
boolTest (Bool _) = Bool True
boolTest _ = Bool False

listTest :: Value -> Value
listTest (List _) = Bool True
listTest _ = Bool False

zeroTest :: Value -> Value
zeroTest (Number x) = Bool $ x == 0
zeroTest (Float x) = Bool $ x == 0
zeroTest _ = Bool False

charTest :: Value -> Value
charTest (Character _) = Bool True
charTest _ = Bool False

symTest :: Value -> Value
symTest (Atom _) = Bool True
symTest _ = Bool False

valNot :: Value -> Value
valNot (Bool False) = Bool True
valNot _ = Bool False

valLength :: Value -> Value
valLength (List xs) = Number $ toInteger (length xs)
valLength _ = Number 0 -- for now

symStr :: Bool -> Value -> Value
symStr True (Atom x) = String $ x
symStr False (String s) = Atom $ s
symStr _ _ = String $ "Could not convert that." -- for now