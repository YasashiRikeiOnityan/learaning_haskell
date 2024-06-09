module Skip where

main :: IO ()
main = do
    [_, x] <- map read . words <$> getLine
    xs <- map read . words <$> getLine
    print $ solve x xs

solve :: Int -> [Int] -> Int
solve x = foldr1 gcd . map (abs . (x -))