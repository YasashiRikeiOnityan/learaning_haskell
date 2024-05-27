module ABC355C where

import Data.Array (Array, listArray, (//), (!))

main :: IO ()
main = do
    [n, t] <- map read . words <$> getLine :: IO [Int]
    as <-  map read . words <$> getLine :: IO [Int]
    print $ bingo (array2D n (card n n)) as n t

card :: Int -> Int -> [[Int]]
card k 1 = [[1..k]]
card k n = card k (n - 1) ++ [[x | j <- [1..k], let x = k * (n - 1) + j]]

bingo :: Array (Int, Int) Int -> [Int] -> Int -> Int -> Int
bingo _ [] _ _ = -1
bingo card_ (x : xs) n t = if isBingo n card_ then t - 1 - length xs else bingo (setZero (coordinate n x) card_) xs n t

coordinate :: Int -> Int -> (Int, Int)
coordinate n a = if a `mod` n == 0 then (a `div` n, n) else (a `div` n + 1, a `mod` n)

array2D :: Int -> [[Int]] -> Array (Int, Int) Int
array2D n as = listArray ((1, 1), (n, n)) (concat as)

setZero :: (Int, Int) -> Array (Int, Int) Int -> Array (Int, Int) Int
setZero (i, j) arr = arr // [((i, j), 0)]

isBingo :: Int -> Array (Int, Int) Int -> Bool
isBingo n arr = 0 `elem` rowSums || 0 `elem` colSums || 0 `elem` diagSums
    where
        rowSums = [sum [arr ! (i, j) | j <- [1..n]] | i <- [1..n]]
        colSums = [sum [arr ! (i, j) | i <- [1..n]] | j <- [1..n]]
        diagSums = [sum [arr ! (i, i) | i <- [1..n]], sum [arr ! (i, n - i + 1) | i <- [1..n]]]
