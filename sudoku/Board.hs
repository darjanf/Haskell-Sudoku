module Board where

import Control.Monad
import System.Random
import Data.List
import Debug.Trace

-- Type-Definitions
type RowIndex       = Int
type ColumnIndex    = Int
type CellIndex      = Int
type Cell           = Int
type Row            = [Cell]
type Column         = [Cell]
type Square         = [Cell]
type Board          = [Row]
type Difficulty     = Int
type Attempts       = Int
type Candidate      = Int
type NoCandidates   = [Candidate] 

initBoard :: IO Board
initBoard = do
    initRow <- genInitRow [] 9
    genBoardRows [initRow] [] 30

genInitRow :: Row -> Int -> IO Row
genInitRow rs 0      = return rs
genInitRow rs length = do
    r <- randomRIO (1,9)
    if r `elem` rs
    then genInitRow rs length
    else genInitRow (r : rs) (length - 1)

fillBoard :: Board -> IO Board
fillBoard [] = fillBoard (replicate 9 $ replicate 9 0)
fillBoard b
    | trace ("calling isBoardFull!") (isBoardFull b) = return b
    | otherwise     = trace ("calling fillBoardCells!") (fillBoardCells b 0 [])

isBoardFull :: Board -> Bool
isBoardFull b
    | null (filter (\x -> elem 0 x) b) = True
    | otherwise                        = False

fillBoardCells :: Board -> CellIndex -> NoCandidates -> IO Board
fillBoardCells b 81 _        = return b
fillBoardCells b cinx noCand = 
    let row       = div cinx 9
        col       = mod cinx 9
        cellValue = getCellValue b (row+1) (col+1)
        emptyCell = cellValue == 0
    in
        if emptyCell then 
            getRandomNumber ([1..9] \\ noCand) >>= \random ->
            let usedInRow     = isUsedInRow    b row random
                usedInColumn  = isUsedInColumn b col random
                usedInSquare  = isUsedInSquare b row col random
                invalidRandom = usedInRow && usedInColumn && usedInSquare
            in 
                if invalidRandom then fillBoardCells b cinx (random:noCand)
                else 
                    let newBoard = setCellValue b (row+1) (col+1) random
                    in
                        fillBoardCells newBoard (cinx+1) []
        else 
            fillBoardCells b (cinx+1) []

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

genBoardRows :: Board -> Row -> Attempts -> IO Board
genBoardRows board row 0        = error "Maximum Attempts reached! Could not generate Sudoku-Board!"
genBoardRows board row attempts =
    if length board < 9 then 
        if length row < 9 then
            calcNewRow board row >>= \candidateIORow ->
            case candidateIORow of
                Nothing -> 
                    --print ("genBoardRows: Invalid Row. Retry with complete new Line!") >>
                    genBoardRows board [] (attempts-1)
                Just cr -> genBoardRows board cr attempts
        else
            case validateRow row of
                True  -> genBoardRows (board ++ [row]) [] attempts
                False -> error "Invalid Row!"
    else 
        return board

calcNewRow :: Board -> Row -> IO (Maybe Row)
calcNewRow board row =
    --print ("-----------------> Start") >>
    --print ("Board: "            ++ show board) >>
    --print ("Row: "              ++ show row) >>
    getAvailableSquareNumbers board row >>= \sqNumbers ->
    let columnNumbers   = getAvailableColumnNumbers board row
        available       = filter (\x -> x `elem` columnNumbers) sqNumbers \\ row
    in
        --print ("availableSqNumbers: "        ++ show sqNumbers) >>
        --print ("availableColumnNumbers: "    ++ show columnNumbers) >>
        --print ("onlyAvailable: "        ++ show available) >>
        --if length board == 3 then error "breakpoint!" else
        if available /= [] then
            --print ("Enough available numbers, select randomnumber next") >>
            getRandomNumber available >>= \random ->
            --print ("Choosed randomnumber: " ++ show random) >>
            --print ("<----------------- End")>>
            return (Just (row ++ [random]))
        else
            --print ("Not enough available numbers, return invalid row!") >>
            --print ("<----------------- End")>>
            return Nothing

getAvailableSquareNumbers :: Board -> Row -> IO [Int]
getAvailableSquareNumbers [] _ = return [1..9]
getAvailableSquareNumbers board row =
    let 
        current3BoardRows   = getCurrent3BoardRows board (length board)
        fstSquare           = getFstSquareNumbers current3BoardRows ++ take 3 row
        sndSquare           = getSndSquareNumbers current3BoardRows ++ take 3 (snd (splitAt 3 row))
        thrdSquare          = getThrdSquareNumbers current3BoardRows ++ snd (splitAt 6 row)
        lengthCurrentRow    = length row
    in
        --print ("current3BoardRows: " ++ show current3BoardRows) >>
        --print ("fstSquare: " ++ show fstSquare) >> 
        --print ("sndSquare: " ++ show sndSquare) >>
        --print ("thrdSquare: " ++ show thrdSquare) >>
        if lengthCurrentRow <= 2 then return $ filter (`notElem` fstSquare)  [1..9]
            else if lengthCurrentRow <= 5 then return $ filter (`notElem` sndSquare)  [1..9]
                else return $ filter (`notElem` thrdSquare) [1..9]

getRandomNumber :: [Int] -> IO Int
getRandomNumber []          = error "no available numbers"
getRandomNumber available = do
    random <- randomRIO (1,9) :: IO Int
    if random `elem` available 
    then return random 
    else getRandomNumber available

getFstSquareNumbers :: Board -> [Int]
getFstSquareNumbers []  = []
getFstSquareNumbers b   = foldr (\(x:y:z:zs) r -> x:y:z:r) [] b

getSndSquareNumbers :: Board -> [Int]
getSndSquareNumbers [] = []
getSndSquareNumbers b = foldr (\(_:_:_:x:y:z:zs) r -> x:y:z:r) [] b

getThrdSquareNumbers :: Board -> [Int]
getThrdSquareNumbers [] = []
getThrdSquareNumbers b = foldr (\(_:_:_:_:_:_:x:y:z:zs) r -> x:y:z:r) [] b

getCurrent3BoardRows :: Board -> Int -> Board
getCurrent3BoardRows [] _  = []
getCurrent3BoardRows board boardLenght 
    | boardLenght <= 2  = fst (splitAt 3 board)
    | boardLenght <= 5  = fst (splitAt 3 (snd (splitAt 3 board)))
    | otherwise         = snd (splitAt 3 (snd (splitAt 3 board)))

getAvailableColumnNumbers :: Board -> Row -> [Int]
getAvailableColumnNumbers [] _ = [1..9]
getAvailableColumnNumbers b r = 
    let lengthRow = length r 
        unavailable = foldr (\x r -> head (snd (splitAt lengthRow x)) : r) [] b
    in filter (`notElem` unavailable) [1..9]

validateRow :: Row -> Bool
validateRow row = case row of
    []      -> True
    (x:xs)  -> x `notElem` xs && validateRow xs

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