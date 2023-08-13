import Data.List
import System.IO

sorted :: [Int] -> Int -> Bool
sorted arr index
    | (index == (length arr - 1)) = True
    | otherwise = (arr!!(index) <= arr!!(index + 1))
         && (sorted arr (index + 1))

-- (gap, last)

gapEvaluation :: (Int, Int) -> Int -> (Int, Int)
gapEvaluation  (gap, last) nextElem = 
    ((min gap (nextElem - last)), nextElem)

smallestGap :: [Int] -> Double
smallestGap arr = fromIntegral (fst (foldl gapEvaluation ((maxBound :: Int, arr!!0)) (drop 1 arr)))

sepIndex :: String -> Char -> Int -> Int
sepIndex str sep index 
    | (index == (length str)) = -1
    | (str!!index == sep) = index
    | otherwise = sepIndex str sep (index + 1)

subString :: Int -> Int -> String -> String
subString left right str = take (right - left + 1) (drop left str)

splitBySpace :: String -> Int -> [Int]
splitBySpace str index = do
    let spaceIndex = sepIndex str ' ' index

    if spaceIndex == -1 then [read 
        (subString index (length str - 1) str) :: Int]
    else [read (subString index (spaceIndex - 1) str) :: Int]
        ++ (splitBySpace str (spaceIndex + 1))

getAndSolve :: Int -> IO ()
getAndSolve times
    | (times == 0) = return () -- print nothing
    | otherwise = do
        n <- getLine
        arrStr <- getLine
        let arr = splitBySpace arrStr 0
        
        if (sorted arr 0) then do 
            print(ceiling ((smallestGap arr + 1) / 2))
            getAndSolve (times - 1)
        else do
            print(0)
            getAndSolve (times - 1)

main = do
    t <- getLine
    getAndSolve (read t :: Int)