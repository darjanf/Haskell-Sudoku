module QuizBoard where

import Board
import Control.Monad
import System.Random
import Data.List

genQuizBoard :: Board -> IO Board
genQuizBoard b = removeNumbers b 15

removeNumbers :: Board -> Difficulty -> IO Board
removeNumbers b 0    = return b
removeNumbers b diff =
    getRandomNumber [1..9] >>= \row ->
    getRandomNumber [1..9] >>= \col ->
    if getCellValue b row col /= 0 then
        let candidate = setCellValue b row col 0
            solveable = isQuizSolveable candidate
        in
            if solveable then
                removeNumbers candidate (diff-1)
            else
                removeNumbers b diff
    else 
        removeNumbers b diff

isQuizSolveable :: Board -> Bool
isQuizSolveable b = True