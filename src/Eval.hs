module Eval where
import ValueLib
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO

apply :: Value -> [Value] -> IOThrowsError Value
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env


applyProc :: [Value] -> IOThrowsError Value
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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
              ("string->symbol", symStr False),
              ("list->string", liStr True),
              ("string->list", liStr False),
              ("=", numBoolBinOp (==)),
              ("/=", numBoolBinOp (/=)),
              ("<", numBoolBinOp (<)),
              (">", numBoolBinOp (>)),
              ("<=", numBoolBinOp (<=)),
              (">=", numBoolBinOp (>=)),
              ("&&", boolBoolBinOp (&&)),
              ("||", boolBoolBinOp (||)),
              ("string=?", strBoolBinOp (==)),
              ("string>?", strBoolBinOp (>)),
              ("string<?", strBoolBinOp (<)),
              ("string<=?", strBoolBinOp (<=)),
              ("string>=?", strBoolBinOp (>=)),
              ("char=?", charBoolBinOp (==)),
              ("char>?", charBoolBinOp (>)),
              ("char<?", charBoolBinOp (<)),
              ("char>=?", charBoolBinOp (>=)),
              ("char<=?", charBoolBinOp (<=)),
              ("cons", cons),
              ("car", car),
              ("cdr", cdr),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("make-string", makeString),
              ("string", stringFromChars),
              ("string-length", strLength),
              ("string-ref", strRef),
              ("substring", subStr),
              ("string-append", stringFromStrings),
              ("string-copy", strCpy)]

ioPrimitives :: [(String, [Value] -> IOThrowsError Value)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

eval :: Env -> Value -> IOThrowsError Value
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
                                              result <- eval env pred
                                              case result of
                                                Bool False -> eval env alt
                                                otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
                                      func <- eval env function
                                      argVals <- mapM (eval env) args
                                      apply func argVals
eval env val@(List s) = return val
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form " badForm

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . show

intBinOp :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsError Value
intBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
intBinOp op params = mapM unpackInt params >>= return . Number . foldl1 op

numericBinOp :: (Float -> Float -> Float) -> [Value] -> ThrowsError Value
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unpackFloat params >>= return . Float . foldl1 op

boolBinOp :: (Value -> ThrowsError a) -> (a -> a -> Bool) -> [Value] -> ThrowsError Value
boolBinOp unpacker op args@[_,_] = do
                                    left <- unpacker $ args !! 0
                                    right <- unpacker $ args !! 1
                                    return $ Bool $ op left right
boolBinOp _ _ any = throwError $ NumArgs 2 any

numBoolBinOp = boolBinOp unpackFloat
strBoolBinOp = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool
charBoolBinOp = boolBinOp unpackChar

unpackInt :: Value -> ThrowsError Integer
unpackInt (Number n) = return n
unpackInt notNum = throwError $ TypeMismatch "Number" notNum

unpackFloat :: Value -> ThrowsError Float
unpackFloat (Float n) = return n
unpackFloat (Number n) = return $ fromIntegral n
unpackFloat notFloat = throwError $ TypeMismatch "Number or Float" notFloat

unpackStr :: Value -> ThrowsError String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "String" notStr

unpackBool :: Value -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "Boolean" notBool

unpackChar :: Value -> ThrowsError Char
unpackChar (Character c) = return c
unpackChar notChar = throwError $ TypeMismatch "Character" notChar

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

liStr :: Bool -> [Value] -> ThrowsError Value
liStr True [List chars] = stringFromChars chars
liStr False [String s] = return $ List $ map (\c -> Character c) s
liStr True [notList] = throwError $ TypeMismatch "List" notList
liStr False [notStr] = throwError $ TypeMismatch "String" notStr
liStr _ any = throwError $ NumArgs 1 any

car :: [Value] -> ThrowsError Value
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [notList] = throwError $ TypeMismatch "List" notList
car any = throwError $ NumArgs 1 any

cdr :: [Value] -> ThrowsError Value
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [notList] = throwError $ TypeMismatch "List" notList
cdr any = throwError $ NumArgs 1 any

cons :: [Value] -> ThrowsError Value
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons any = throwError $ NumArgs 2 any 

eqv :: [Value] -> ThrowsError Value
eqv [(Bool a), (Bool b)] = return $ Bool $ a == b
eqv [(Atom a), (Atom b)] = return $ Bool $ a == b
eqv [(List a), (List b)] = return $ Bool $ listEq a b
eqv [(DottedList as a), (DottedList bs b)] = return $ Bool $ listEq (as ++ [a]) (bs ++ [b])
eqv [(Number a), (Number b)] = return $ Bool $ a == b
eqv [(Number a), (Float b)] = return $ Bool $ (fromIntegral a) == b
eqv [(Float a), (Number b)] = return $ Bool $ a == (fromIntegral b)
eqv [(Float a), (Float b)] = return $ Bool $ a == b
eqv [(String a), (String b)] = return $ Bool $ a == b
eqv [(Character a), (Character b)] = return $ Bool $ a == b
eqv [_,_] = return $ Bool False
eqv any = throwError $ NumArgs 2 any

listEq :: [Value] -> [Value] -> Bool
listEq xs ys | length xs /= length ys = False
             | otherwise = and (map eqvPair $ zip xs ys)
                where
                    eqvPair (x,y) = case eqv [x,y] of
                        Left err -> False
                        Right (Bool val) -> val

makeString :: [Value] -> ThrowsError Value
makeString [(Number n), (Character c)] = return $ String $ map (\_ -> c) [1..n]
makeString any@[_,_] = throwError $ TypeMismatch "a Number and Character" (List any)
makeString any = throwError $ NumArgs 2 any

stringFromChars :: [Value] -> ThrowsError Value
stringFromChars chars = mapM unpackChar chars >>= return . String

stringFromStrings :: [Value] -> ThrowsError Value
stringFromStrings strings = mapM unpackStr strings >>= return . String . foldl1 (++)

strLength :: [Value] -> ThrowsError Value
strLength [(String s)] = return $ Number $ toInteger (length s)
strLength [notStr] = throwError $ TypeMismatch "String" notStr
strLength any = throwError $ NumArgs 2 any

strRef :: [Value] -> ThrowsError Value
strRef [(String s), (Number n)] | n < 0 || n > (toInteger ((length s) - 1)) = throwError $ Default ("Number must be between 0 and " ++ (show ((length s) - 1)) ++ ".")
                                | otherwise = return $ Character $ s !! (fromIntegral n)
strRef notArgs@[_,_] = throwError $ TypeMismatch "a String and a Number" (List notArgs)
strRef any = throwError $ NumArgs 2 any

subStr :: [Value] -> ThrowsError Value
subStr [(String s), (Number start), (Number end)] | start < 0 = throwError $ Default "Start must be 0 or more."
                                                  | end > (toInteger ((length s) - 1)) = throwError $ Default ("End must be less than " ++ show ((length s) - 1) ++ ".")
                                                  | otherwise = return $ String $ drop (fromIntegral start) (take (fromIntegral end) s)
subStr any@[_,_,_] = throwError $ TypeMismatch "a String, a start Number, and an end Number" (List any)
subStr any = throwError $ NumArgs 3 any

strCpy :: [Value] -> ThrowsError Value
strCpy [val@(String s)] = return val
strCpy [notStr] = throwError $ TypeMismatch "String" notStr
strCpy any = throwError $ NumArgs 1 any

makePort :: IOMode -> [Value] -> IOThrowsError Value
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort mode [notStr] = throwError $ TypeMismatch "String" notStr
makePort _ any = throwError $ NumArgs 1 any

closePort :: [Value] -> IOThrowsError Value
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort [_] = return $ Bool False
closePort any = throwError $ NumArgs 1 any

readProc :: [Value] -> IOThrowsError Value
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc [notPort] = throwError $ TypeMismatch "Port" notPort
readProc any = throwError $ NumArgs 1 any

writeProc :: [Value] -> IOThrowsError Value
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc any = throwError $ NumArgs 2 any

readContents :: [Value] -> IOThrowsError Value
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents [notStr] = throwError $ TypeMismatch "String" notStr
readContents any = throwError $ NumArgs 1 any

load :: String -> IOThrowsError [Value]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [Value] -> IOThrowsError Value
readAll [String filename] = liftM List $ load filename
readAll [notStr] = throwError $ TypeMismatch "String" notStr
readAll any = throwError $ NumArgs 1 any
