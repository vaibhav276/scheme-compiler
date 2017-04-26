module Main where

import Lib
import System.Environment
import Control.Monad (liftM)

import Repl (runRepl, evalAndPrint)

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> evalAndPrint $ args !! 0
        otherwise -> putStrLn "Program takes only 0 or 1 argument"
