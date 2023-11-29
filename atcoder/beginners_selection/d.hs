main :: IO ()
main = do
    _ <- getLine
    as <- map read . words <$> getLine :: IO [Int]
    let ans = if all even as then maxDivisibilityBy2 as else 0
    print ans

maxDivisibilityBy2 :: [Int] -> Int
maxDivisibilityBy2 xs = if not (all even xs) then 0 else 
    let ys = map (`div` 2) xs
    in if all even ys && notElem 0 ys then 1 + maxDivisibilityBy2 ys else 1