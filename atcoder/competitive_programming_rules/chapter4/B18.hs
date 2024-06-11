module B18 where

import Data.Bifunctor ( bimap )

judge :: Int -> [(Int, Int)] -> [(Int, [Int])] -> [(Int, [Int])]
judge _ [] bs = bs
judge s (a : as) bs = judge s as (partialMap s a bs)

partialMap :: Int -> (Int, Int) -> [(Int, [Int])] -> [(Int, [Int])]
partialMap _ _ [] = []
partialMap s x [y]
    | snd x + fst y > s = [y]
    | otherwise = y : [bimap (snd x +) (fst x :) y]
partialMap s x (y : z : zs)
    | snd x + fst y > s = y : partialMap s x (z : zs)
    | snd x + fst y == s = [(s, fst x : snd y)]
    | snd x + fst y == fst z = partialMap s x (z : zs)
    | otherwise = y : bimap (snd x +) (fst x :) y : partialMap s x (z : zs)

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
    putStrLn $ b18 s (zip [1..] as)