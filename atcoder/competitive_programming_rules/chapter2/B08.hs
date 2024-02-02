module B08 where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray, accumArray, elems, bounds )

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
 where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n

fromArrayCS :: Array (Int, Int) Int -> Array (Int, Int) Int
fromArrayCS as = do
  let rows = chunksOf w $ elems as
      ss = scanl (zipWith (+)) (replicate (w + 1) 0) $ map (scanl (+) 0) rows
  listArray (bounds as) (concat ss)
  where
    ((lh, lw), (uh, uw)) = bounds as
    w = uw + 1 - lw


twoDimensionalSum :: Int -> Int -> [[Int]] -> Array (Int, Int) Int
twoDimensionalSum h w xs = listArray ((0, 0), (h, w))
                         $ concat
                         $ scanl1 (zipWith (+))
                         $ map (scanl1 (+)) xs

arrayToLists :: Array (Int, Int) Int -> [[Int]]
arrayToLists arr = [[arr ! (i, j) | j <- [0..jMax]] | i <- [0..iMax]]
    where rightBound = snd (bounds arr)
          iMax = fst rightBound
          jMax = snd rightBound

solve :: Array (Int, Int) Int -> [(Int, Int, Int, Int)] -> [Int]
solve twoDimSum = map (\(a, b, c, d) -> twoDimSum ! (a - 1, b - 1) + twoDimSum ! (c, d) 
                                      - twoDimSum ! (c, b - 1) - twoDimSum ! (a - 1, d))

main :: IO ()
main = do
    n <- readLn
    xys <- map ((\[x, y] -> ((x, y), 1)) . map read . words) <$> replicateM n getLine
    q <- readLn
    qs <- map ((\[a, b, c, d] -> (a, b, c, d)) . map read . words) <$> replicateM q getLine
    -- let h = maximum $ map (\((x, _), _) -> x) xys
    -- let w = maximum $ map (\((_, y), _) -> y) xys
    -- mapM_ print $ solve (twoDimensionalSum h w $ arrayToLists $ accumArray (+) 0 ((0, 0), (h, w)) xys) qs
    mapM_ print $ solve (fromArrayCS $ accumArray (+) 0 ((0, 0), (1500, 1500)) xys) qs


{-
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