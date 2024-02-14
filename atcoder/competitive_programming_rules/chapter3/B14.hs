module B14 where

import Data.List ( subsequences, sort )
import Data.Array ( Array, listArray, (!))

divList :: Int -> [Int] -> ([Int], [Int])
divList len = splitAt (len `div` 2)

possibleSums :: [Int] -> [Int]
possibleSums xs = map sum (subsequences xs)

binarySearch :: Int -> Int -> Array Int Int -> Int -> Bool
binarySearch l r arr x
    | l > r = False
    | x == arr ! mid = True
    | x < arr ! mid = binarySearch l (mid - 1) arr x
    | otherwise = binarySearch (mid + 1) r arr x
        where
            mid = (l + r) `div` 2

main :: IO ()
main = do
    [n, k] <- map read . words <$> getLine
    as <- map read . words <$> getLine
    let (firstHalf, secondHalf) = divList n as
        firstSums = possibleSums firstHalf
        secondSums = listArray (1, 2 ^ (n - (n `div` 2))) (sort $ possibleSums secondHalf)
    putStrLn $ if any (binarySearch 1 (2 ^ (n - (n `div` 2))) secondSums . (k -)) firstSums then "Yes" else "No"