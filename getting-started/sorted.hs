import Data.List
import System.IO


arr = 3 : 5 : 6 : 15 : []

sorted ::  [Int] -> Bool

sorted arr = ((length arr <= 1) || (head arr <= head(drop 1 arr) && 
    (sorted (drop 1 arr))))


