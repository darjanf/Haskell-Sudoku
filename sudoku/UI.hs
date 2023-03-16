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

_TOP_EDGE_ :: [String]
_TOP_EDGE_ = replicate _SIZE_ "_"

_BOTTOM_EDGE_ :: [String]
_BOTTOM_EDGE_ = replicate _SIZE_ "-"

_RANGE_ :: [Int]
_RANGE_ = [0..(_SIZE_ - 1)]

_SIZE_ :: Int
_SIZE_ = 9

printBoard :: Board -> IO ()
printBoard b = putStrLn $ formatBoard b

formatLine :: [Int] -> String
formatLine x = concat ["| ", intercalate " | " (showInts x), " |"]

formatSquareLine :: [Int] -> String
formatSquareLine x = concat ["|_", intercalate _SEP_ (showInts x), "_|"]

_HEADER_ :: String
_HEADER_ = concat [" -", intercalate "---" _BOTTOM_EDGE_, "-"]

_BOTTOM_ :: String
_BOTTOM_ = concat [" -", intercalate "---" _BOTTOM_EDGE_, "-"]

showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs)
    | x == 0     = " " : showInts xs
    | otherwise  = show x : showInts xs

prependRowIndices :: [String] -> [String]
prependRowIndices s = zipWith (:) ['A'..] s

formatRows :: [Row] -> Int -> [String]
formatRows [] _ = []
formatRows [x] _ = [formatLine x]
formatRows (x:xs) inx
    | inx `elem` [2,5,8] = formatSquareLine x : formatRows xs (inx + 1)
    | otherwise          = formatLine x : formatRows xs (inx + 1)
--    map (\x -> formatLine x) rl

formatBoard :: Board -> String
formatBoard b = unlines $ ((_HEADER_ : (formatRows b 0)) ++ [_BOTTOM_])
    --unlines $ _HEADER_ : prependRowIndices (formatRows b)