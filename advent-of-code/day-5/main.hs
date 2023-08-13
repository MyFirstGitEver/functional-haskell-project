import Data.Tuple
import System.IO



subString :: Int -> Int -> String -> String
subString l r str = (take (r - l + 1) (drop l str))

checkRecentDifferent :: String -> Int -> Int
checkRecentDifferent str index
    | (subStringDifferent (subString index (index + 3) str)) = (index + 3)
    | otherwise = checkRecentDifferent str (index + 1)

main = do
    x <- readFile "input.txt"
    print(checkRecentDifferent x 0)