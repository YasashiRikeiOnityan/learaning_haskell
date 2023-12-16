just :: Int -> Int -> [Int] -> Bool
just _ _ [] = False
just 1 num xs = num `elem` xs
just 2 num (x : xs) = any (\m -> m + x == num) xs || just 2 num xs
just k num as@(x : xs) = length as >= k && k >= 0
    && (just (k - 1) (num - x) xs || just k num xs)

main :: IO ()
main = do
    _ <- read <$> getLine :: IO Int
    as <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if just 3 1000 as then "Yes" else "No"