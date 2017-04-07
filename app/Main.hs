module Main where

import Lib
import Parser
import System.Environment

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
