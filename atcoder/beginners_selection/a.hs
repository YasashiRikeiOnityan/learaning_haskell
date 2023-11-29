main :: IO ()
main = do
    [a] <- map read . words <$> getLine :: IO [Int]
    [b, c] <- map read . words <$> getLine :: IO [Int]
    s <- getLine
    putStrLn $ show (a + b + c) ++ " " ++ s