module ABC341C where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray, elems )

twoDimensionalSum :: Int -> Int -> [[Int]] -> Array (Int, Int) Int
twoDimensionalSum h w xs = listArray ((0, 0), (h, w))
                         $ concat
                         $ scanl (zipWith (+)) (replicate (w + 1) 0)
                         $ map (scanl (+) 0) xs

solve :: Int -> Array (Int, Int) Int -> Int
solve n arr = length . filter (== n) $ elems arr

form :: Int -> Int -> [String] -> Array (Int, Int) Int
form h w ss = listArray ((1, 1), (h, w)) $ concatMap (map (\s -> if s == '#' then 0 else 1)) ss

main :: IO ()
main = do
    [h, w, n] <- map read . words <$> getLine
    t <- getLine
    ss <- map words <$> replicateM h getLine
    print . solve n . form h w $ ss
