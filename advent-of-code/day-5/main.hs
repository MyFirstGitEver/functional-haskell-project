import Data.Tuple
import System.IO

checkCharFrom :: Char -> Int -> Int -> String -> Bool
checkCharFrom ch index to str
    | (index == to + 1) = True
    | (ch == str!!index) = False
    | otherwise = checkCharFrom ch (index + 1) to str

isAllDistinct :: String -> Int -> Bool
isAllDistinct str index
    | (index == (length str)) = True
    | (not (checkCharFrom (str!!index) (index + 1) (length str - 1) str)) = False
    | otherwise = isAllDistinct str (index + 1)

subString :: Int -> Int -> String -> String
subString l r str = (take (r - l + 1) (drop l str))

solve :: String -> Int -> Int
solve str index
    | (isAllDistinct (subString index (index + 13) str) 0) = (index + 13)
    | otherwise = (solve str (index + 1))

main = do
    x <- readFile "input.txt"
    print(solve x 0)