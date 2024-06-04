module ABC350C where

import Data.Array ( listArray, (!), Array, assocs, elems )
import Data.Maybe ( listToMaybe, fromJust )

main :: IO ()
main = do
    n <- readLn :: IO Int
    as <- map read . words <$> getLine :: IO [Int]
    let ans = sortWay 1 as []
    print $ length ans
    mapM_ (\(a, b) -> putStrLn $ show a ++ " " ++ show b) ans

sortWay :: Int -> [Int] -> [(Int, Int)] -> [(Int, Int)]
sortWay _ [] way = way
sortWay k as way = sortWay (succ k) tail way'
    where
        arr = listArray (1, length as) as
        min = minimum as
        id = indexOf arr min
        way' = if k == fromJust id + pred k then way else way ++ [(k, fromJust id + pred k)]
        tail = [x | x <- elems arr, x /= min]

indexOf :: (Eq a) => Array Int a -> a -> Maybe Int
indexOf arr val = listToMaybe [i | (i, v) <- assocs arr, v == val]