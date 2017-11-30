module Lib where
import REPL

-- Exposed functions
evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

runRepl :: IO ()
runRepl = until_ exit (readPrompt "coco~>>> ") evalAndPrint

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
