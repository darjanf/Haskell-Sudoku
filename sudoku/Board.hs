module Board where

import Control.Monad
import System.Random
import Data.List

-- Type-Definitions
type Row        = [Int]
type Board      = [Row]
type Square     = Int 

genInitRow :: Row -> Int -> IO Row
genInitRow rs 0      = return rs
genInitRow rs length = do
    r <- randomRIO (1,9)
    if r `elem` rs
    then genInitRow rs length
    else genInitRow (r : rs) (length - 1)

getRandomNumber :: [Int] -> IO Int
getRandomNumber available = do
    random <- randomRIO (1,9) :: IO Int
    if null available || random `elem` available 
    then return random 
    else getRandomNumber available

calcNewRow :: Board -> Row -> IO Row
calcNewRow board row =
    let 
        available = getAvailableSquareNumbers (reshapeBoard board) \\ row
    in do
        print ("Board: " ++ show (reshapeBoard board))
        print ("Row: " ++ show (row))
        print ("GetAvailableSquareNumbers: " ++ show (getAvailableSquareNumbers (reshapeBoard board)))
        print ("Available: " ++ show (available))
        random <- getRandomNumber available
        return (row ++ [random])

dropBoardRows :: Board -> Int -> Board
dropBoardRows [] _ = []
dropBoardRows b n
  | n `elem` [1,2] = b
  | n `elem` [4,5] = drop 3 b
  | n `elem` [7,8] = drop 6 b
  | otherwise = []

reshapeBoard :: Board -> Board
reshapeBoard [] = []
reshapeBoard b = dropBoardRows b (length b)

getAvailableSquareNumbers :: Board -> [Int]
getAvailableSquareNumbers [] = [1..9]
getAvailableSquareNumbers board = 
    filter (`notElem` unavailableSquareNumbers) [1..9]
    where unavailableSquareNumbers = foldr (\(x:y:z:zs) r -> x:y:z:r) [] board

genBoardRows :: Board -> Row -> IO Board
genBoardRows board row =
    if length board < 9 then 
        if length row < 9 then do
            candidateRow <- calcNewRow board row
            genBoardRows board candidateRow
        else
            genBoardRows (board ++ [row]) []
    else 
        return board
genBoard :: IO Board
genBoard = do
    initRow <- genInitRow [] 9
    let 
        initBoard = [initRow]
    genBoardRows initBoard []

initBoard :: IO Board
initBoard = genBoard