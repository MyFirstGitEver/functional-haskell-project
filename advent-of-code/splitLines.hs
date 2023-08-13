import Data.List
import System.IO

indexOfNewline :: [Char] -> Int -> Int
indexOfNewline str i
    | (i == (length str - 1)) =  -1
    | (str!!i == '\n') = i
    | otherwise = indexOfNewline str (i + 1)

subString:: Int -> Int -> [Char] -> [Char]
subString l r str = take (r - l + 1) (drop l str)

splitLines:: String -> Int -> [String]

splitLines str from = do
    let newLineIndex = indexOfNewline str from

    if newLineIndex == -1 then [subString from (length str - 1) str]
    else [(subString from (newLineIndex - 1) str)] ++ splitLines str (newLineIndex + 1)

main = do
    x <- readFile "input.txt"
    print(splitLines x 0)