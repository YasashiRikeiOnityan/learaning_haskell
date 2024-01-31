module A08 where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray )

twoDimensionalSum :: Int -> Int -> [[Int]] -> Array (Int, Int) Int
twoDimensionalSum h w xs = listArray ((0, 0), (h, w))
                         $ concat
                         $ scanl (zipWith (+)) (replicate (w + 1) 0)
                         $ map (scanl (+) 0) xs

solve :: Array (Int, Int) Int -> [[Int]] -> [Int]
solve twoDimSum = map (\[x, y, z, w] -> twoDimSum ! (x - 1, y - 1) + twoDimSum ! (z, w) 
                                      - twoDimSum ! (z, y - 1) - twoDimSum ! (x - 1, w))

main :: IO ()
main = do
    [h, w] <- map read . words <$> getLine :: IO [Int]
    xs <- map (map read . words) <$> replicateM h getLine :: IO [[Int]]
    q <- readLn :: IO Int
    qs <- map (map read . words) <$> replicateM q getLine :: IO [[Int]]
    mapM_ print $ solve (twoDimensionalSum h w xs) qs
