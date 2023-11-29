main :: IO ()
main = do
    [n, k] <- map read . words <$> getLine :: IO [Int]
    ps <- map read . words <$> getLine :: IO[Int]
    qs <- map read . words <$> getLine :: IO [Int]
    if any (\m -> elem k $ map (+ m) qs) ps then putStrLn "Yes" else putStrLn "No"