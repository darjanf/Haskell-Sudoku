module Main where

import Board
import QuizBoard
import UI
import Data.List
import Data.Ord
import Text.Printf
import Control.Monad

main :: IO ()
main =
    putStrLn "Welcome to Haskell Sudoku!" >>
    --initBoard >>= \fullBoard ->
    fillBoard Nothing >>= \mFullBoard ->
        case mFullBoard of
            Nothing -> error "Could not generate Board!"
            Just fullBoard -> 
                genQuizBoard fullBoard >>= \quizBoard ->
                printBoard quizBoard