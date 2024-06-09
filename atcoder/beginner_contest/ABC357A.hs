main :: IO ()
main = do
    [n, m] <- map read . words <$> getLine
    hs <- map read . words <$> getLine
    print $ n - disinfection m hs

disinfection :: Int -> [Int] -> Int
disinfection _ [] = 0
disinfection k hs@(x : xs)
    | k >= x = disinfection (k - x) xs
    | otherwise = length hs