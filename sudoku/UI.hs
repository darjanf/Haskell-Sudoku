module UI where

import Board
import Data.List
import Data.List.Index
import Data.Ord
import Text.Printf
import Control.Monad

-- Constants
_SEP_ :: String
_SEP_ = "_|_"

_EDGE_ :: String
_EDGE_ = take 27 (repeat '-')

_NUMBERS_ :: [String]
_NUMBERS_ = map show [1..9]

_RANGE_ :: [Int]
_RANGE_ = [0..(_SIZE_ - 1)]

_SIZE_ :: Int
_SIZE_ = 9

_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= \a -> putStrLn a

printBoard :: Board -> IO ()
printBoard b = putStrLn $ formatBoard b

formatLine :: [Int] -> Int -> String
formatLine x inx = concat [" ", show inx, "    | ", intercalate "" (showInts x "  "), " | "]

formatSepLine :: String
formatSepLine = concat ["  ", "    | ", _EDGE_, " | "]

_HEADER1_ :: String
_HEADER1_ = concat [
            "    Col ", 
            intercalate "  " (map show [1..3]), 
            take 3 (repeat ' '), 
            intercalate "  " (map show [4..6]),
            take 3 (repeat ' '), 
            intercalate "  " (map show [7..9]),
            " "]

_HEADER2_ :: String
_HEADER2_ = concat ["Row    -", _EDGE_, "- "]

_BOTTOM_ :: String
_BOTTOM_ = concat ["       -", _EDGE_, "-"]

showInts :: [Int] -> String -> [String]
showInts [] _ = []
showInts (x:xs) sep
    | length xs `elem` [6,3] = val : " | " : showInts xs sep
    | length xs `elem` [0]   = val : showInts xs sep
    | otherwise              = val : sep : showInts xs sep
    where
        val = if (x == 0) then " " else show x

showIntsSquare :: [Int] -> String -> [String]
showIntsSquare [] _ = []
showIntsSquare (x:xs) sep
    | length xs `elem` [6,3] = val : "_|_" : showIntsSquare xs sep
    | length xs `elem` [0]   = val : showIntsSquare xs sep    
    | otherwise              = val : sep : showIntsSquare xs sep
    where
        val = if(x == 0) then " " else show x

prependRowIndices :: [String] -> [String]
prependRowIndices s = zipWith (:) ['A'..] s

formatRows :: [Row] -> Int -> [String]
formatRows [] _ = []
formatRows [x] inx = [formatLine x (inx + 1)]
formatRows (x:xs) inx
    | inx `elem` [2,5,8] = formatLine x (inx + 1) : formatSepLine : formatRows xs (inx + 1)
    | otherwise          = formatLine x (inx + 1) : formatRows xs (inx + 1)
--    map (\x -> formatLine x) rl
-- | inx `elem` [2,5,8] = formatSquareLine x (inx + 1) : formatSepLine : formatRows xs (inx + 1)

formatBoard :: Board -> String
formatBoard b = unlines $ ((_HEADER1_: _HEADER2_ : (formatRows b 0)) ++ [_BOTTOM_])
    --unlines $ _HEADER_ : prependRowIndices (formatRows b)