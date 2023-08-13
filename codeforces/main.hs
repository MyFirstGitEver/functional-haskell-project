import Data.List
import System.IO

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

counter :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
counter (answer, cnt, k, prev) current = do
    if current - prev <= k 
        then (max (cnt + 1) answer, cnt + 1, k, current)
    else (max cnt answer, 1, k, current)
 
getFirst (a,_,_,_) = a

getAndSolve :: Int -> IO ()
getAndSolve times 
    | (times == 0) = return ()
    | otherwise = do
        nAndK <- getLine
        arrStr <- getLine

        let arr = splitBySpace arrStr 0

        if (length arr == 1) then do
            print(0)
            getAndSolve (times - 1)
        else do
            let k = (splitBySpace nAndK 0)!!1
            let sortedArr = sort arr

            print((length arr) - getFirst(
                foldl counter (1, 1, k, sortedArr!!0) (drop 1 sortedArr))) 
            getAndSolve (times - 1)

main = do
    t <- getLine
    getAndSolve (read t :: Int)