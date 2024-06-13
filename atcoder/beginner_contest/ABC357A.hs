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

{-

| --------------------------------- |
| disinfection 10 [2, 3, 2, 5, 3] = |
| disinfection  8    [3, 2, 5, 3] = |
| disinfection  5       [2, 5, 3] = |
| disinfection  3          [5, 3] = |
| length [5, 3] = 2                 |
|                                   |
| thus, ans = 5 - 2 = 3             |
| --------------------------------- |

-}
