{-
import Control.Monad ( replicateM_ )

cumulativeSum :: [Int] -> [Int]
cumulativeSum = scanl (+) 0

solve :: [Int] -> [Int] -> Int
solve s [l, r] = (s !! r) - (s !! (l - 1))

main :: IO ()
main = do
    [_, q] <- map read . words <$> getLine :: IO [Int]
    as <- map read . words <$> getLine :: IO [Int]
    let cumSum = cumulativeSum as
    replicateM_ q $ do
        lr <- map read . words <$> getLine :: IO [Int]
        print $ solve cumSum lr
-}

import Control.Monad ( replicateM_ )
import Data.Array ( Array, (!), listArray )

cumulativeSum :: Int -> Int -> [Int] -> Array Int Int
cumulativeSum l r as = listArray (l, r) $ scanl (+) 0 as

solve :: Array Int Int -> [Int] -> Int
solve s [l, r] = (s ! r) - (s ! (l - 1))
solve _ _ = -1

main :: IO ()
main = do
    [n, q] <- map read . words <$> getLine :: IO [Int]
    as <- map read . words <$> getLine :: IO [Int]
    let cumSum = cumulativeSum 0 n as
    replicateM_ q $ do
        lr <- map read . words <$> getLine :: IO [Int]
        print $ solve cumSum lr