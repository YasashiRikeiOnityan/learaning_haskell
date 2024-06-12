module A19 where

import Control.Monad ( replicateM )
import Data.Bifunctor ( bimap )

knapsack :: Int -> [(Int, Int)] -> [(Int, Int)] -> Int
knapsack _ [] vs = maximum_ 0 vs
knapsack c (wv : wvs) vs = knapsack c wvs (partialMap c wv vs)

partialMap :: Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
partialMap _ (_, _) [] = []
partialMap c (w, v) (x : xs)
    | w + fst x > c = x : partialMap c (w, v) xs
    | otherwise = x : bimap (w +) (v +) x : partialMap c (w, v) xs

maximum_ :: Int -> [(Int, Int)] -> Int
maximum_ = foldl (\ m x -> max m (snd x))

toPair :: [Int] -> (Int, Int)
toPair [x, y] = (x, y)
toPair _ = (0, 0)

main :: IO ()
main = do
    [n, w] <- map read . words <$> getLine :: IO [Int]
    wvs <- map (toPair . map read . words) <$> replicateM n getLine :: IO [(Int, Int)]
    print $ knapsack w wvs [(0, 0)]

{-
4 7
3 13
3 17
5 29
1 10

40
-}

{-
type Capacity = Int
type Value = Int
type Weight = Int
data Item = Item { weight :: Weight, value :: Value } deriving Show
data KSProblem = KSP { capacity :: Capacity, items :: [Item]}

knapsack :: KSProblem -> Value
knapsack (KSP _ []) = 0
knapsack (KSP c (i : is))
    | weight i <= c = max (knapsack (KSP c is))
                          (value i + knapsack (KSP (c - weight i) is))
    | otherwise = knapsack (KSP c is)

toItem :: [Int] -> Item
toItem [x, y] = Item x y
toItem _ = Item 0 0

main :: IO ()
main = do
    [n, w] <- map read . words <$> getLine :: IO [Int]
    wvs <- map (toItem . map read . words) <$> replicateM n getLine
    print 0
-}
