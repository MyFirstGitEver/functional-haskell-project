import Data.List
import System.IO

insertAtN :: [Int] -> Int -> Int -> [Int]

arr = [1, 2, 3]

insertAtN  arr value position = ((take position arr) ++ [value] ++
    (drop position arr))