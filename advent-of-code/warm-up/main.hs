import Data.Tuple
import System.IO

indexOfNewline :: [Char] -> Int -> Int
indexOfNewline str i
    | (i == (length str - 1)) =  -1
    | (str!!i == '\n') = i
    | otherwise = indexOfNewline str (i + 1)

subString:: Int -> Int -> String -> String
subString l r str = take (r - l + 1) (drop l str)

splitLines:: String -> Int -> [String]
splitLines str from = do
    let newLineIndex = indexOfNewline str from

    if newLineIndex == -1 then [subString from (length str - 1) str]
    else [(subString from (newLineIndex - 1) str)] ++ splitLines str (newLineIndex + 1)

next :: (Int, Int) -> String -> (Int, Int)
next (a, b) str
    | (str == "") = (max a b, 0)
    | otherwise = (a, b + (read str :: Int))

main = do
    x <- readFile "input.txt"
    print(foldl next (0, 0) (splitLines x 0))