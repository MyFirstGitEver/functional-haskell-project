import Data.Tuple
import System.IO

subStringOf4Different :: String -> Bool
subStringOf4Different str = (
    str!!0 /= str!!1 && str!!0 /= str!!2 && str!!0 /= str!!3 &&
    str!!1 /= str!!2 && str!!1 /= str!!3 && str!!2 /= str!!3)

subString :: Int -> Int -> String -> String
subString l r str = (take (r - l + 1) (drop l str))

checkRecentDifferent :: String -> Int -> Int
checkRecentDifferent str index
    | (subStringOf4Different (subString index (index + 3) str)) = (index + 3)
    | otherwise = checkRecentDifferent str (index + 1)

main = do
    x <- readFile "input.txt"
    print(checkRecentDifferent x 0)