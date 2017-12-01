module Main where
import Control.Monad
import System.Environment
import Lib

main :: IO ()
main = do
    args <- getArgs
    if null args then intro runRepl else runOne $ args
