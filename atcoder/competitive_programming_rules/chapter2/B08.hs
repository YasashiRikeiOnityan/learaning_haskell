module B08 where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray, accumArray, bounds )

twoDimensionalSum :: Int -> Int -> [[Int]] -> Array (Int, Int) Int
twoDimensionalSum h w xs = listArray ((0, 0), (h, w))
                         $ concat
                         $ scanl1 (zipWith (+))
                         $ map (scanl1 (+)) xs

arrayToLists :: Array (Int, Int) Int -> [[Int]]
arrayToLists arr = [[arr ! (i, j) | j <- [0..snd (snd (bounds arr))]] | i <- [0..fst (snd (bounds arr))]]

solve :: Array (Int, Int) Int -> [(Int, Int, Int, Int)] -> [Int]
solve twoDimSum = map (\(a, b, c, d) -> twoDimSum ! (a - 1, b - 1) + twoDimSum ! (c, d) 
                                      - twoDimSum ! (c, b - 1) - twoDimSum ! (a - 1, d))

main :: IO ()
main = do
    n <- readLn :: IO Int
    xys <- map ((\[x, y] -> ((x, y), 1)) . map read . words) <$> replicateM n getLine
    let h = maximum $ map (\((x, _), _) -> x) xys
    let w = maximum $ map (\((_, y), _) -> y) xys
    q <- readLn :: IO Int
    qs <- map ((\[a, b, c, d] -> (a, b, c, d)) . map read . words) <$> replicateM q getLine
    mapM_ print $ solve (twoDimensionalSum h w $ arrayToLists $ accumArray (+) 0 ((0, 0), (h, w)) xys) qs


{-
    let w = 1500 -- maximum $ map (\((x, _), _) -> x) xys
    let h = 1500 -- maximum $ map (\((_, y), _) -> y) xys
5
1 3
2 5
3 4
2 6
3 3
3
1 3 3 6
1 5 2 6
1 3 3 5

-}