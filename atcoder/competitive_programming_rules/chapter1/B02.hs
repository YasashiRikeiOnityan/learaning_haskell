main :: IO ()
main = do
    [a, b] <- map read . words <$> getLine :: IO [Int]
    let x = [a..b]
    putStrLn $ if 0 `elem` map (100 `mod`) x then "Yes" else "No"