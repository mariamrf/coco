module Eval where
import ValueLib
import Control.Monad.Except

apply :: String -> [Value] -> ThrowsError Value
apply func args = maybe (throwError $ NotFunction func) ($ args) $ lookup func primitives

primitives :: [(String, [Value] -> ThrowsError Value)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp (/)),
              ("mod", intBinOp (mod)),
              ("quotient", intBinOp (quot)),
              ("remainder", intBinOp (rem)),
              ("number?", numTest),
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

intBinOp :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsError Value
intBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
intBinOp op params = mapM unpackInt params >>= return . Number . foldl1 op

numericBinOp :: (Float -> Float -> Float) -> [Value] -> ThrowsError Value
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unpackFloat params >>= return . Float . foldl1 op

unpackInt :: Value -> ThrowsError Integer
unpackInt (Number n) = return n
unpackInt notNum = throwError $ TypeMismatch "Number" notNum

unpackFloat :: Value -> ThrowsError Float
unpackFloat (Float n) = return n
unpackFloat (Number n) = return $ fromIntegral n
unpackFloat notFloat = throwError $ TypeMismatch "Number or Float" notFloat

numTest :: [Value] -> ThrowsError Value
numTest [Number _] = return $ Bool True
numTest [Float _] = return $ Bool True
numTest [_] = return $ Bool False
numTest any = throwError $ NumArgs 1 any

strTest :: [Value] -> ThrowsError Value
strTest [String _] = return $ Bool True
strTest [_] = return $ Bool False
strTest any = throwError $ NumArgs 1 any

boolTest :: [Value] -> ThrowsError Value
boolTest [Bool _] = return $ Bool True
boolTest [_] = return $ Bool False
boolTest any = throwError $ NumArgs 1 any

listTest :: [Value] -> ThrowsError Value
listTest [List _] = return $ Bool True
listTest [_] = return $ Bool False
listTest any = throwError $ NumArgs 1 any

zeroTest :: [Value] -> ThrowsError Value
zeroTest [Number x] = return $ Bool $ x == 0
zeroTest [Float x] = return $ Bool $ x == 0
zeroTest [_] = return $ Bool False
zeroTest any = throwError $ NumArgs 1 any

charTest :: [Value] -> ThrowsError Value
charTest [Character _] = return $ Bool True
charTest [_] = return $ Bool False
charTest any = throwError $ NumArgs 1 any

symTest :: [Value] -> ThrowsError Value
symTest [Atom _] = return $ Bool True
symTest [_] = return $ Bool False
symTest any = throwError $ NumArgs 1 any

valNot :: [Value] -> ThrowsError Value
valNot [Bool False] = return $ Bool True
valNot [_] = return $ Bool False
valNot any = throwError $ NumArgs 1 any

valLength :: [Value] -> ThrowsError Value
valLength [List xs] = return $ Number $ toInteger (length xs)
valLength [notList] = throwError $ TypeMismatch "List" notList
valLength any = throwError $ NumArgs 1 any

symStr :: Bool -> [Value] -> ThrowsError Value
symStr True [Atom x] = return $ String $ x
symStr False [String s] = return $ Atom $ s
symStr True [notAtom] = throwError $ TypeMismatch "Atom" notAtom
symStr False [notStr] = throwError $ TypeMismatch "String" notStr
symStr _ any = throwError $ NumArgs 1 any
