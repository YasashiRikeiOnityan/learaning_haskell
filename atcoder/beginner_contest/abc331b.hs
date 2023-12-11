module Main where

main :: IO ()
main = do
    [n, s, m, l] <- map read . words <$> getLine :: IO [Int]

    print ""

howManyEggs :: [Int] -> Int -> Int
howManyEggs es max = 0