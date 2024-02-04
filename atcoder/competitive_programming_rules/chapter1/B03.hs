module B03 where

just :: Int -> Int -> [Int] -> Bool
just _ _ [] = False
just 1 num xs = num `elem` xs
just k num (x : xs) = just (k - 1) (num - x) xs || just k num xs

main :: IO ()
main = do
    _ <- readLn :: IO Int
    as <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if just 3 1000 as then "Yes" else "No"