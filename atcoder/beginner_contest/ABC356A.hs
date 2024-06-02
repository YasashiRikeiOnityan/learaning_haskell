module ABC356A where
    
main :: IO ()
main = do
    [n, l, r] <- map read . words <$> getLine :: IO [Int]
    putStrLn . unwords . map show $ take (pred l) [1..n] ++ reverse [l..r] ++ drop r [1..n]