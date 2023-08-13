import Data.List
import System.IO

findFirstLarger :: [Int] -> Int -> Int

findFirstLarger arr value
    | length arr < 1 = -1
    | head arr > value = (head arr)
    | otherwise = findFirstLarger (drop 1 arr) value