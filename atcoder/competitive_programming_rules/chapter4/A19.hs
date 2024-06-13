module A19 where

import Control.Monad ( replicateM )
import Data.Bifunctor ( bimap )

knapsack :: Int -> [(Int, Int)] -> [(Int, Int)] -> Int
knapsack _ [] vs = snd . head $ vs
knapsack c ((w, v) : wvs) vs = knapsack c wvs . merge vs . filter ((<= c) . fst) . map (bimap (w +) (v +)) $ vs

merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
merge [] wvs = wvs
merge wvs [] = wvs
merge wvs1@((w1, v1) : wv1) wvs2@((w2, v2) : wv2) = case compare w1 w2 of
    LT -> case compare v1 v2 of
        LT -> (w2, v2) : merge wvs1 wv2
        _  -> merge wvs1 wv2
    EQ -> (w1, max v1 v2) : merge wv1 wv2
    GT -> case compare v1 v2 of
        GT -> (w1, v1) : merge wv1 wvs2
        _  -> merge wv1 wvs2

toPair :: [Int] -> (Int, Int)
toPair [x, y] = (x, y)
toPair _ = (0, 0)

main :: IO ()
main = do
    [n, w] <- map read . words <$> getLine
    wvs <- map (toPair . map read . words) <$> replicateM n getLine
    print $ knapsack w wvs [(0, 0)]