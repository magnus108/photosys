module Main where

import Lib
import Text.Read
import System.Environment (getArgs)


main :: IO ()
main = do
    [port] <- getArgs
    someFunc (read port)
