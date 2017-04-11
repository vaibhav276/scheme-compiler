module Main where

import Lib
import Parser
import System.Environment
import Control.Monad (liftM)

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
