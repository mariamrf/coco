module Main where
import Control.Monad
import System.Environment
import Lib

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> intro runRepl
        1 -> evalAndPrint $ args !! 0
        otherwise -> putStrLn "Program takes only 0 or 1 arguments."
