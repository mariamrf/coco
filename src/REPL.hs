module REPL where
import System.IO hiding (try)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import Data.IORef
import ValueLib
import Eval


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

readExpr :: String -> ThrowsError Value
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

nullEnv :: IO Env
nullEnv = newIORef []

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
    where
        makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
                              result <- prompt
                              if pred result
                                then return ()
                                else action result >> until_ pred prompt action

exit :: String -> Bool
exit arg = (arg == ":quit") || (arg == ":q")
