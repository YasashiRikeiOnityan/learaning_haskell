main :: IO ()
main = do
    [n, a, b] <- map read . words <$> getLine :: IO [Int]
    let t = [s | s <- [1..n], let w = sum (int2list s) in a <= w && w <= b]
    let ans = sum t
    print ans

int2list :: Int -> [Int]
int2list n = map (\c -> read [c] :: Int) (show n)