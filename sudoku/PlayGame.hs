{-# Language FlexibleContexts #-}

module PlayGame where

import Board
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe
import QuizBoard
import System.Random
import Text.Printf
import UI

-- ************************************************************
-- Main functions
-- ************************************************************

playGame :: StatefulGame
playGame = do
    game      <- get
    case isBoardFull $ gQuizGame game of
        True  -> do
            liftIO finishGame
        False -> do
            liftIO (printLogo)
            liftIO (printBoard $ gQuizGame game)
            userInput <- liftIO (getUserInput)
            procInput userInput
            liftIO (threadDelay 2000000)
            playGame

-- ************************************************************
-- Helper functions
-- ************************************************************

procInput :: UserInput -> StatefulGame
procInput (row,col,val) = do
    game <- get
    let initBoard  = gInitGame game
        quizBoard  = gQuizGame game
        iCellValue = getCellValue initBoard row col
        qCellValue = getCellValue quizBoard row col
        action
            | qCellValue /= 0 = do
                liftIO (printf "Cell already filled! Try again ...\n")
            | iCellValue == val = do
                liftIO (printf "Input is Valid! Board is going to be updated ...\n\n")
                let newBoard = setCellValue quizBoard row col val
                modify (\g -> g { gQuizGame = newBoard})
            | iCellValue /= val = do
                liftIO (printf "Input is invalid! Try again ...\n")
            | otherwise       = error "Input could not be processed!"
    action


createGame :: Maybe Board -> Board -> Game
createGame initBoard quizBoard = MkGame { gInitGame = fromJust initBoard, gQuizGame = quizBoard }

getUserInput :: IO (UserInput)
getUserInput = do
    printf "Please enter your guess in the format 'Row Column Value' (e.g. 1 4 9)\n"
    input <- getLine
    let splitList = splitOn " " input
        row       = read (splitList !! 0) :: Int
        col       = read (splitList !! 1) :: Int
        val       = read (splitList !! 2) :: Int
    return (row,col,val)

finishGame :: IO ()
finishGame = printf "You solved the Sudoku. Congratulations !!!\n\n\n"