main :: IO ()
main = do
    [a, b] <- map read . words <$> getLine :: IO [Int]
    let ans = if even (a * b) then "Even" else "Odd"
    putStrLn ans
