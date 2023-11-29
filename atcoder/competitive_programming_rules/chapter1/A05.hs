main :: IO ()
main = do
    [n, k] <- map read . words <$> getLine :: IO [Int]
    print $ length [(a, b, c) | a <- [1..n], b <- [1..n], let c = k - a - b, 1 <= c, c <= n]