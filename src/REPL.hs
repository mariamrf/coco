module REPL where
import System.IO hiding (try)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import ValueLib
import Eval

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

readExpr :: String -> ThrowsError Value
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

eval :: Value -> ThrowsError Value
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
                                              result <- eval pred
                                              case result of
                                                Bool False -> eval alt
                                                otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval val@(List s) = return val
eval badForm = throwError $ BadSpecialForm "Unrecognized special form " badForm

trapError action = catchError action (return . show)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
                              result <- prompt
                              if pred result
                                then return ()
                                else action result >> until_ pred prompt action

exit :: String -> Bool
exit arg = (arg == ":quit") || (arg == ":q")
