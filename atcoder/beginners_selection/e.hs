main :: IO ()
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    c <- readLn :: IO Int
    x <- readLn :: IO Int
    let ans = solve a b c x
    print ans

solve :: Int -> Int -> Int -> Int -> Int
solve a b c x = if not (a < 0 || b < 0 || c < 0 || x < 0)
    then length $ filter (== x) [500 * a' + 100 * b' + 50 * c' | a' <- [0..a], b' <- [0..b], c' <- [0..c]]
    else 0