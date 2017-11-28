module Main where

import System.Environment
import Lib

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
