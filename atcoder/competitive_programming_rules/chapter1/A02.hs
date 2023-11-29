main :: IO ()
main = do
    [_, x] <- map read . words <$> getLine :: IO [Int]
    as <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if x `elem` as then "Yes" else "No"