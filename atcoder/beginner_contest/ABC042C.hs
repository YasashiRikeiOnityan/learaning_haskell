module ABC042C where

searchMin :: [String] -> [[String]] -> [String]
searchMin ds (x : xs) = if any (`elem` ds) x then searchMin ds xs else x

main :: IO ()
main = do
    [n, k] <- map read . words <$> getLine :: IO [Int]
    ds <- map read . words <$> getLine :: IO [Int]
    let lists = map show [n..]
    print "" -- $ searchMin (map show ds) lists