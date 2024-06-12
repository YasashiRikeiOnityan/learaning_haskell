module A19 where

import Control.Monad ( replicateM )
import Data.Bifunctor ( bimap )

knapsack :: Int -> [(Int, Int)] -> [(Int, Int)] -> Int
knapsack c [] vs = snd . last $ vs
knapsack c (wv : wvs) vs = knapsack c wvs (partialMap c wv vs)

knapsack' :: Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
knapsack' c [] vs = vs
knapsack' c (wv : wvs) vs = knapsack' c wvs (partialMap c wv vs)

partialMap :: Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
partialMap _ (_, _) [] = []
partialMap c (w, v) [x]
    | w + fst x > c = [x]
    | otherwise = x : [bimap (w +) (v +) x]
partialMap c (w, v) (x : y : ys) 
    | w + fst x > c = partialMap c (w, v) (y : ys)
    | otherwise = case (v + snd x) `compare` snd y of
        LT -> x : bimap (w +) (v +) x : partialMap c (w, v) (y : ys) 
        EQ -> partialMap c (w, v) (y : ys)
        GT -> x : y : bimap (w +) (v +) x : bimap (w +) (v +) y : partialMap c (w, v) ys

toPair :: [Int] -> (Int, Int)
toPair [x, y] = (x, y)
toPair _ = (0, 0)

main :: IO ()
main = do
    [n, w] <- map read . words <$> getLine :: IO [Int]
    wvs <- map (toPair . map read . words) <$> replicateM n getLine :: IO [(Int, Int)]
    -- print $ knapsack w wvs [(0, 0)]
    print $ knapsack' w wvs [(0, 0)]

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
