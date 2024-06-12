module JumpingTakahashi where

import Control.Monad ( replicateM )

toPair :: [Int] -> [(Int, Int)]
toPair [x, y] = [(x, y)]
toPair _ = []

jump :: Int -> ([(Int, Int)], [Int]) -> [Int]
jump _ ([], list) = list
jump x (ab : abs', list) = jump x (abs', [i + v | i <- list, v <- [fst ab, snd ab], i + v <= x, (i + v) `notElem` list])

main :: IO ()
main = do
    [n, x] <- map read . words <$> getLine
    abs' <- concatMap (toPair . map read . words) <$> replicateM n getLine
    putStrLn $ if x `elem` jump x (abs', [0]) then "Yes" else "No"