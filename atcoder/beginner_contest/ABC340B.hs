module ABC340B where

import Control.Monad ( replicateM )

toPair :: [Int] -> [(Int, Int)]
toPair [x, y] = [(x, y)]
toPair _ = []

solve :: [(Int, Int)] -> [Int] -> [Int] -> [Int]
solve [] _ ans = ans
solve (q : qs) list ans = case q of
    (1, x) -> solve qs (list ++ [x]) ans
    (2, k) -> solve qs list (ans ++ [reverse list !! (k - 1)])
    _ -> solve qs list ans

main :: IO ()
main = do
    q <- readLn
    qs <- concatMap (toPair . map read . words) <$> replicateM q getLine
    mapM_ print (solve qs [] [])