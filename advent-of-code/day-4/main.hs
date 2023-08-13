import Data.Tuple
import System.IO

subString :: Int -> Int -> String -> String
subString left right str = take (right - left + 1) (drop left str)

sepIndex :: String -> Char -> Int -> Int
sepIndex str sep index
    | (str!!index == sep) = index
    | otherwise = (sepIndex str sep (index + 1))

splitByComma :: String -> (String, String)
splitByComma str = do
    let index = sepIndex str ',' 0
    ((take index str), (drop (index + 1) str))

splitNumsByLine :: String -> (Int, Int)
splitNumsByLine str = do
    let index = sepIndex str '-' 0
    ((read (take index str) :: Int), 
        (read (drop (index + 1) str) :: Int))

getOverlapPoint :: (Int, Int) -> (Int, Int) -> Int
getOverlapPoint (a, b) (c, d)
    | ((a >= c && b <= d)) = 1
    | ((c >= a && d <= b)) = 1
    | otherwise = 0


splitLines :: String -> Int -> [String]
splitLines str index = do
    let newlineIndex = sepIndex str '\n' index
    if (newlineIndex == ((length str) - 1)) then 
        [subString index ((length str) - 2) str]
    else [subString index (newlineIndex - 1) str] ++ 
        splitLines str (newlineIndex + 1)

count:: Int -> String -> Int
count result str = do
    let tup = splitByComma str
    let firstIntTup = splitNumsByLine (fst tup)
    let secondIntTup = splitNumsByLine (snd tup)

    (getOverlapPoint firstIntTup secondIntTup) + result

main = do
    x <- readFile "input-1"
    print(foldl count 0 (splitLines x 0))