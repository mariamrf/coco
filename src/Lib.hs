module Lib where
import REPL
import ValueLib
import Eval
import Control.Monad
import System.IO

-- Exposed functions
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: [String] -> IO ()
runOne args = do
                env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
                (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
                        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ exit (readPrompt "coco~>>> ") . evalAndPrint

intro next = do   
                putStrLn "    ___       ___       ___       ___   "
                putStrLn "   /\\  \\     /\\  \\     /\\  \\     /\\  \\  "
                putStrLn "  /::\\  \\   /::\\  \\   /::\\  \\   /::\\  \\ "
                putStrLn " /:/\\:\\__\\ /:/\\:\\__\\ /:/\\:\\__\\ /:/\\:\\__\\"
                putStrLn " \\:\\ \\/__/ \\:\\/:/  / \\:\\ \\/__/ \\:\\/:/  /"
                putStrLn "  \\:\\__\\    \\::/  /   \\:\\__\\    \\::/  / "
                putStrLn "   \\/__/     \\/__/     \\/__/     \\/__/  "
                putStrLn "                                        "
                putStrLn "    Scheme (Lisp) Interpreter, R5RS     "
                putStrLn "  {exit: \":q\", github: @mariamrf/coco}"
                putStrLn "                                        "
                next
