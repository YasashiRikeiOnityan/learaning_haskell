module B18 where

import Data.Bifunctor ( bimap )

judge :: Int -> [(Int, Int)] -> [(Int, [Int])] -> [(Int, [Int])]
judge _ [] bs = bs
judge s (a : as) bs = judge s as (partialMap s a bs)

partialMap :: Int -> (Int, Int) -> [(Int, [Int])] -> [(Int, [Int])]
partialMap _ _ [] = []
partialMap s x [y]
    | fst x + fst y > s = [y]
    | otherwise = y : [bimap (fst x +) (snd x :) y]
partialMap s x (y : z : zs)
    | fst x + fst y > s = y : partialMap s x (z : zs)
    | fst x + fst y == s = [(s, snd x : snd y)]
    | fst x + fst y == fst z = partialMap s x (z : zs)
    | otherwise = y : bimap (fst x +) (snd x :) y : partialMap s x (z : zs)

check :: Int -> [(Int, [Int])] -> (Int, [Int])
check _ [] = (-1, [])
check s (y : ys)
    | fst y == s = y
    | otherwise = check s ys

b18 :: Int -> [(Int, Int)] -> String
b18 s as = if fst res == s then (show . length . snd $ res) ++ "\n" ++ (unwords . map show . reverse $ snd res) else "-1"
    where res = check s $ judge s as [(0, [])]

main :: IO ()
main = do
    [_, s] <- map read . words <$> getLine
    as <- map read . words <$> getLine
    putStrLn $ b18 s (zip as [1..])