import Data.List (sort)

main :: IO ()
main = do
    n <- readLn :: IO Int
    as <- map read . words <$> getLine :: IO [Int]
    let ans = solve n (sort as)
    print ans

solve :: Int -> [Int] -> Int
solve 1 [a] = a
solve n (a : as) = if odd n then a + solve (n - 1) as else -a + solve (n - 1) as
solve _ _ = 0