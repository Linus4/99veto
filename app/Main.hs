module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= handleArgs
