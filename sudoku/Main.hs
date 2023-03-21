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
    hSetBuffering stdout NoBuffering
    solvedBoard <- initBoard Nothing
    quizBoard   <- genQuizBoard solvedBoard
    runStateT playGame (createGame solvedBoard quizBoard)
    return ()