module Board where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State
import Data.List
import Debug.Trace
import System.Random

-- Type-Definitions
type Board           = [Row]
type Candidate       = Int
type Cell            = Int
type CellIndex       = Int
type Column          = [Cell]
type ColumnIndex     = Int
type Difficulty      = Int
type NoCandidates    = [Candidate]
type Row             = [Cell]
type RowIndex        = Int
type SolutionCount   = Int
type Square          = [Cell]
type StatefulGame    = StateT Game IO ()
type StatefulMaybe a = StateT BoardCreation Maybe a
type UserInput       = (Int,Int,Int)

-- Data-Definitions
data BoardCreation = MkBoard {
                    bSuperFunction  :: StatefulMaybe Board,
                    bInitBoard      :: Board, 
                    bCurrentBoard   :: Board, 
                    bCellIndex      :: CellIndex,
                    bNoCandidates   :: NoCandidates
                   }
data Game          = MkGame {
                    gInitGame       :: Board,
                    gQuizGame    :: Board 
                   }

-- ************************************************************
-- Main functions
-- ************************************************************

initBoard :: Maybe Board -> IO (Maybe Board)
initBoard Nothing  = genInitRow [] 9 >>= \initRow -> 
                     initBoard (Just (initRow : (replicate 8 $ replicate 9 0)))
initBoard (Just b) = if isBoardFull b
                     then return (Just b)
                     else
                        let initState = MkBoard { 
                                            bSuperFunction = fillBoardCells, 
                                            bInitBoard = b, 
                                            bCurrentBoard = b,
                                            bNoCandidates = [],
                                            bCellIndex = 0
                                        }
                            sResult = runStateT fillBoardCells initState
                        in
                            case sResult of
                                Nothing      -> return Nothing      -- failed to generate board
                                Just (nB, _) -> return (Just nB)    -- return generated board

fillBoardCells :: StatefulMaybe Board
fillBoardCells = do
    game <- get
    let cinx         = bCellIndex game
        currentBoard = bCurrentBoard game
        action
            | cinx == 81 = return currentBoard  -- recursion ends when field index is 81
            | otherwise  = solveBoard           -- start new recursion
    action

checkBoardSolutions :: StatefulMaybe Board
checkBoardSolutions = do
    game <- get
    let cinx      = bCellIndex game
        initBoard = bInitBoard game
        quizBoard = bCurrentBoard game
        action
            | cinx == 81 = if (initBoard == quizBoard) 
                           then fail "try to find other solutions"
                           else return quizBoard
            | otherwise  = solveBoard
    action

-- ************************************************************
-- Solver functions
-- ************************************************************

solveBoard :: StatefulMaybe Board
solveBoard = do
    game <- get
    let superFunction       = bSuperFunction game
        cinx                = bCellIndex game
        noCand              = bNoCandidates game
        initBoard           = bInitBoard game
        currentBoard        = bCurrentBoard game
        row                 = calcRowIndex cinx
        col                 = calcColumnIndex cinx
        cellValue           = getCellValue currentBoard (row+1) (col+1)
        emptyCell           = cellValue == 0
        availableNumbers    = [1..9] \\ noCand
        action
            | (not emptyCell) = do  -- cell is already filled, skip this cell
                g <- get
                let newInx = (bCellIndex g) + 1
                modify (\g -> g { bCellIndex = newInx })
                superFunction
            | emptyCell && (null availableNumbers) = fail "no solution found"
            | otherwise                            =
                let 
                    random       = head availableNumbers -- get new candidate value for empty cell
                    usedInRow    = isUsedInRow    currentBoard row random
                    usedInColumn = isUsedInColumn currentBoard col random
                    usedInSquare = isUsedInSquare currentBoard row col random
                    validRandom  = (not usedInRow) && (not usedInColumn) && (not usedInSquare)
                in  
                    if validRandom then
                        let newBoard = setCellValue currentBoard (row+1) (col+1) random
                            b1State = 
                                MkBoard { 
                                    bSuperFunction = superFunction, 
                                    bInitBoard     = initBoard, 
                                    bCurrentBoard  = newBoard,
                                    bCellIndex     = cinx+1,
                                    bNoCandidates  = []
                                }
                            b1Result = runStateT superFunction b1State
                        in 
                            case b1Result of 
                                Just (board1, _) -> return board1
                                Nothing          -> do
                                    modify (\g -> g { bNoCandidates = random:noCand })
                                    solveBoard

                    else do
                        modify (\g -> g { bNoCandidates = random:noCand })
                        solveBoard
    action

-- ************************************************************
-- Helper functions
-- ************************************************************

genInitRow :: Row -> Int -> IO Row
genInitRow rs 0      = return rs
genInitRow rs length = do
    r <- randomRIO (1,9)
    if r `elem` rs
    then genInitRow rs length
    else genInitRow (r : rs) (length - 1)

isBoardFull :: Board -> Bool
isBoardFull b
    | null (filter (\x -> elem 0 x) b) = True
    | otherwise                        = False

calcRowIndex :: CellIndex -> RowIndex
calcRowIndex cInx = div cInx 9

calcColumnIndex :: CellIndex -> ColumnIndex
calcColumnIndex cInx = mod cInx 9

isUsedInRow :: Board -> RowIndex -> Candidate -> Bool
isUsedInRow b row cand
    | (elem cand $ getRow b row) = True
    | otherwise                  = False

isUsedInColumn :: Board -> ColumnIndex -> Candidate -> Bool
isUsedInColumn b col cand
    | (elem cand $ getColumn b col) = True
    | otherwise                     = False

isUsedInSquare :: Board -> RowIndex -> ColumnIndex -> Candidate -> Bool
isUsedInSquare b rIndex cIndex cand
    | (elem cand $ getSquare b rIndex cIndex)   = True
    | otherwise                                 = False

getRandomNumber :: [Int] -> IO Int
getRandomNumber []          = error "no available numbers"
getRandomNumber available = do
    random <- randomRIO (1,9) :: IO Int
    if random `elem` available 
    then return random 
    else getRandomNumber available

getCellValue :: Board -> RowIndex -> ColumnIndex -> Cell
getCellValue b row col = cell
    where 
        bRow = b !! (row-1)
        cell = bRow !! (col-1)

setCellValue :: Board -> RowIndex -> ColumnIndex -> Cell -> Board
setCellValue b row col cell = changedBoard
    where   sBoard          = splitAt (row-1) b
            bRow            = head $ snd sBoard
            sRow            = splitAt (col-1) bRow
            changedRow      = concat [fst sRow, [cell], tail $ snd sRow]
            changedBoard    = concat [fst sBoard, [changedRow], tail $ snd sBoard]

getRow :: Board -> RowIndex -> Row
getRow b rIndex = row
    where   sBoard = splitAt rIndex b
            row    = head $ snd sBoard

getColumn :: Board -> ColumnIndex -> Column
getColumn b cIndex = foldr (\x r -> (head $ snd (splitAt cIndex x)) : r) [] b

getSquare :: Board -> RowIndex -> ColumnIndex -> Square
getSquare b rIndex cIndex
    | rIndex < 3 && cIndex < 3  = getSquareByIndex b (0,0)
    | rIndex < 3 && cIndex < 6  = getSquareByIndex b (0,3)
    | rIndex < 3 && cIndex < 9  = getSquareByIndex b (0,6)
    | rIndex < 6 && cIndex < 3  = getSquareByIndex b (3,0)
    | rIndex < 6 && cIndex < 6  = getSquareByIndex b (3,3)
    | rIndex < 6 && cIndex < 9  = getSquareByIndex b (3,6)
    | rIndex < 9 && cIndex < 3  = getSquareByIndex b (6,0)
    | rIndex < 9 && cIndex < 6  = getSquareByIndex b (6,3)
    | rIndex < 9 && cIndex < 9  = getSquareByIndex b (6,6)

getSquareByIndex :: Board -> (RowIndex, ColumnIndex) -> Square
getSquareByIndex b (rIndex,cIndex) = concat square
    where
        rows  = take 3 $ snd (splitAt rIndex b)
        square = foldr (\x r -> (take 3 $ snd (splitAt cIndex x)) : r) [] rows