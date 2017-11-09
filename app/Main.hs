module Main where

import System.Environment
import Lib

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
