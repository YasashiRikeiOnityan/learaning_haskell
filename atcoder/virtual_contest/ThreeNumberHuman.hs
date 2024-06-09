module ThreeNumberHuman where

import Data.List ( sort, nub )

solve :: [Int] -> Int
solve nums =  (!! 2) . nub . reverse . sort $ [x + y + z | x <- nums, y <- nums, x /= y, z <- nums, y /= z, z /= x]

main :: IO ()
main = do
    nums <- map read . words <$> getLine :: IO [Int]
    print $ solve nums