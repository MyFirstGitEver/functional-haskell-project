import Data.List
import System.IO

findFirstLargerIndex :: [Int] -> Int -> Int -> Int
insertAtN :: [Int] -> Int -> Int -> [Int]
insertIntoSorted :: [Int] -> Int -> [Int]

findFirstLargerIndex arr from value
    | length arr < 1 = -1
    | head arr > value = from
    | otherwise = findFirstLargerIndex (drop 1 arr) (from + 1) value

arr = [1, 2, 3]

insertAtN  arr value position = ((take position arr) ++ [value] ++
    (drop position arr))

insertIntoSorted arr value = do
    let index = findFirstLargerIndex arr 0 value
    if index == -1 then (arr ++ [value])
    else insertAtN arr value (findFirstLargerIndex arr 0 value)