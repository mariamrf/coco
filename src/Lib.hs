module Lib where
import REPL
import ValueLib (Env)

-- Exposed functions
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

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
                putStrLn "  {exit: \":q\", github: @mariamrf/coco}"
                putStrLn "                                        "
                next
