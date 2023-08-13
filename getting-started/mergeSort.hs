import Data.List
import System.IO

findFirstLargerIndex :: [Int] -> Int -> Int -> Int
insertAtN :: [Int] -> Int -> Int -> [Int]
insertIntoSorted :: [Int] -> Int -> [Int]
mergeTwo :: [Int] -> [Int] -> [Int]
sorted :: [Int] -> [Int]

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

mergeTwo arr [] = arr
mergeTwo arr1 arr2 = mergeTwo (insertIntoSorted arr1 (head arr2)) 
        (drop 1 arr2)

sorted arr = do
    let half = (length arr) `div` 2

    if length arr < 2 then arr
    else mergeTwo (sorted (take half arr)) (sorted (drop half arr))