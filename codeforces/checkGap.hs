import Data.List
import System.IO

merge :: [Int] -> [Int] -> [Int]

merge [] x = x
merge x [] = x

merge (x:xs) (y:ys) | y < x     = y : merge (x:xs) ys
merge (x:xs) (y:ys) | otherwise = x : merge xs (y:ys)

mergeSort :: [Int] -> [Int]
mergeSort arr = do
    let halfIndex = ((length arr) `div` 2)

    if (length arr == 1) then arr
    else merge (mergeSort (take halfIndex arr)) 
        (mergeSort (drop halfIndex arr))


sepIndex :: String -> Char -> Int -> Int
sepIndex str sep index
    | (index == (length str)) = -1
    | (str!!index == sep) = index
    | otherwise = (sepIndex str sep (index + 1))

subString :: Int -> Int -> String -> String
subString l r str = take (r - l + 1) (drop l str)

splitBySpace :: String -> Int -> [Int]
splitBySpace str index = do
    let spaceIndex = sepIndex str ' ' index

    if spaceIndex == -1 then [read (subString index (length str - 1) str) :: Int]
    else [read (subString index (spaceIndex - 1) str) :: Int] 
        ++ splitBySpace str (spaceIndex + 1)

counter :: (Int, Int, Int, [Int]) -> Int -> (Int, Int, Int, [Int])
counter (lastInvalid, k, index, arr) result = do
    if index < (length arr - 1) && (arr!!(index + 1) - arr!!(index)) > k
        then (index, k, index, arr) 
    else (lastInvalid k, index, arr)

getFirst (a,_,_,_) = a

getAndSolve :: Int -> IO ()
getAndSolve times 
    | (times == 0) = return ()
    | otherwise = do
        nAndK <- getLine
        let k = (splitBySpace nAndK 0)!!1
        arrStr <- getLine
        let arr = splitBySpace arrStr 0
        let sortedArr = mergeSort arr
        print(getFirst(foldl counter (-1, k, 0, sortedArr) sortedArr) + 1) 
        getAndSolve (times - 1)

main = do
    t <- getLine
    getAndSolve (read t :: Int)