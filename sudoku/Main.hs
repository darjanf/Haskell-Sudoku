module Main where

import Board
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Ord
import PlayGame
import QuizBoard
import System.IO
import Text.Printf
import UI

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering                        -- output is written immediately
    solvedBoard <- initBoard Nothing                        -- generate a full board
    quizBoard   <- genQuizBoard solvedBoard                 -- generate a quiz board
    runStateT playGame (createGame solvedBoard quizBoard)   -- play sudoku game
    return ()