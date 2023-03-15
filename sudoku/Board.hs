module Board where

import Control.Monad
import System.Random
import Data.List

-- Type-Definitions
type Row        = [Int]
type Board      = [Row]
type Square     = Int
type Difficulty = Int

genInitRow :: Row -> Int -> IO Row
genInitRow rs 0      = return rs
genInitRow rs length = do
    r <- randomRIO (1,9)
    if r `elem` rs
    then genInitRow rs length
    else genInitRow (r : rs) (length - 1)

getRandomNumber :: [Int] -> IO Int
getRandomNumber []          = error "no available numbers"
getRandomNumber available = do
    random <- randomRIO (1,9) :: IO Int
    if random `elem` available 
    then return random 
    else getRandomNumber available

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

genBoardRows :: Board -> Row -> IO Board
genBoardRows board row =
    if length board < 9 then 
        if length row < 9 then
            calcNewRow board row >>= \candidateIORow ->
            case candidateIORow of
                Nothing -> 
                    --print ("genBoardRows: Invalid Row. Retry with complete new Line!") >>
                    genBoardRows board []
                Just cr -> genBoardRows board cr
        else
            case validateRow row of
                True  -> genBoardRows (board ++ [row]) []
                False -> error "Invalid Row!"
    else 
        return board

genBoard :: IO Board
genBoard = do
    initRow <- genInitRow [] 9
    let initBoard = [initRow]
    genBoardRows initBoard []

initBoard :: IO Board
initBoard = genBoard

genQuizBoard :: Board -> Difficulty -> IO Board
genQuizBoard b d
    | d <= 1 = reduceBoardFields b 2
    | d == 2 = b

reduceBoardFields :: Board -> Int -> Board
reduceBoardFields board _               = []
reduceBoardFields [x] _                 = []
reduceBoardFields (x:y:z:zs) number     = undefined

reduceRowFields :: Row -> Int -> Row
reduceRowFields board _         = []
reduceRowFields [x] _         = []
reduceRowFields board number    = undefined