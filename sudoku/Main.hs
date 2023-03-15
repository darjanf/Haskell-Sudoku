module Main where

import Board
import UI
import Data.List
import Data.Ord
import Text.Printf
import Control.Monad

main :: IO ()
main =
    putStrLn "Welcome to Haskell Sudoku!" >>
    initBoard >>= \board ->
    printBoard board