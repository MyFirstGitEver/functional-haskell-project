import Data.List
import System.IO

sumFrom1To :: Int -> Int

sumFrom1To 0 = 0
sumFrom1To n = n + sumFrom1To (n - 1)