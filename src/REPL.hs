module REPL where
import System.IO hiding (try)
import Control.Monad.Except
import Data.IORef
import ValueLib
import Eval

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

nullEnv :: IO Env
nullEnv = newIORef []

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
    ++ map (makeFunc PrimitiveFunc) primitives)
    where
        makeFunc constructor (var, func) = (var, constructor func)


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
                              result <- prompt
                              if pred result
                                then return ()
                                else action result >> until_ pred prompt action

exit :: String -> Bool
exit arg = (arg == ":quit") || (arg == ":q")
