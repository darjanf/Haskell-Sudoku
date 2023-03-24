module QuizBoard where

import Board
import Control.Monad
import Control.Monad.State
import Data.List
import System.Random


-- ************************************************************
-- Main functions
-- ************************************************************

genQuizBoard :: Maybe Board -> IO (Board)
genQuizBoard Nothing  = error "genQuizBoard: Invalid input!"
genQuizBoard (Just b) = (removeNumbers b b 15)

-- ************************************************************
-- Solver functions
-- ************************************************************

removeNumbers :: Board -> Board -> Difficulty -> IO Board
removeNumbers iB qB 0    = return qB
removeNumbers iB qB diff =
    getRandomNumber [1..9] >>= \row ->
    getRandomNumber [1..9] >>= \col ->
    if getCellValue qB row col /= 0 then
        let 
            candidate = setCellValue qB row col 0
            ioSolveable = isQuizSolveable iB candidate
        in
            ioSolveable >>= \solveable ->
            if solveable then
                removeNumbers iB candidate (diff-1)
            else
                removeNumbers iB qB (diff-1)
    else 
        removeNumbers iB qB diff

-- ************************************************************
-- Helper functions
-- ************************************************************

isQuizSolveable :: Board -> Board -> IO Bool
isQuizSolveable iB qB =
    let initState = 
            MkBoard { 
                bSuperFunction = checkBoardSolutions, 
                bInitBoard = iB, 
                bCurrentBoard = qB,
                bNoCandidates = [],
                bCellIndex = 0
            }
        sResult = runStateT checkBoardSolutions initState
    in 
        case sResult of
            Nothing -> return True
            Just b  -> return False