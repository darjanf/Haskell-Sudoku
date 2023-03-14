module Board where

import Control.Monad
import System.Random

-- Type-Definitions
type Row        = [Int]
type Board      = [Row]

genInitRow :: Row -> Int -> IO Row
genInitRow rs 0      = return rs
genInitRow rs length = do
    r <- randomRIO (1,9)
    if r `elem` rs
    then genInitRow rs length
    else genInitRow (r : rs) (length - 1)

getAvailableNumbers :: Board -> [Int]
getAvailableNumbers [] = [1..9]
getAvailableNumbers b =
    let blength = length b in
        if blength <= 3 then
            let unavailableSquareNumbers = foldr (\(x:y:z:zs) r -> x:y:z:r) [] b
            in
                filter (`notElem` unavailableSquareNumbers) [1..9]
        else 
            if length b <= 6 then
            getAvailableNumbers $ drop 3 b
            else
                getAvailableNumbers $ drop 6 b

genBoardRows :: Board -> IO Board
genBoardRows [] = do
    ir <- genInitRow [] 9
    genBoardRows [ir]
genBoardRows b = undefined


genBoard :: IO Board
genBoard = genBoardRows []

initBoard :: IO Board
initBoard = genBoard