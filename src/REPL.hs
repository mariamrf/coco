module REPL where
import System.IO hiding (try)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import Data.IORef
import ValueLib
import Eval

type Env = IORef [(String, IORef Value)]
type IOThrowsError = ExceptT SchemeError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError Value
getVar envRef var = do
                      env <- liftIO $ readIORef envRef
                      maybe (throwError $ UnboundVar "Getting an unbound variable." var)
                            (liftIO . readIORef)
                            (lookup var env)

setVar :: Env -> String -> Value -> IOThrowsError Value
setVar envRef var val = do
                          env <- liftIO $ readIORef envRef
                          maybe (throwError $ UnboundVar "Setting an unbound variable." var)
                                (liftIO . (flip writeIORef val))
                                (lookup var env)
                          return val

defineVar :: Env -> String -> Value -> IOThrowsError Value
defineVar envRef var val = do
                             alreadyDefined <- liftIO $ isBound envRef var
                             if alreadyDefined
                                then setVar envRef var val >> return val
                                else liftIO $ do
                                    valueRef <- newIORef val
                                    env <- readIORef envRef
                                    writeIORef envRef ((var, valueRef) : env)
                                    return val

bindVars :: Env -> [(String, Value)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
                            where
                                extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
                                addBinding (var, val) = do
                                                           ref <- newIORef val
                                                           return (var, ref)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

readExpr :: String -> ThrowsError Value
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env val@(List s) = return val
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form " badForm

trapError action = catchError action (return . show)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
                              result <- prompt
                              if pred result
                                then return ()
                                else action result >> until_ pred prompt action

exit :: String -> Bool
exit arg = (arg == ":quit") || (arg == ":q")
