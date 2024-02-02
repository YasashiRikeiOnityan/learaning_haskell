module A07 where

import Control.Monad ( replicateM )
import Data.Array ( accumArray, elems, Array )

add2PreviousDay :: [Int] -> [(Int, Int)]
add2PreviousDay [l, r] = [(l, 1), (r + 1, -1)]
add2PreviousDay _ = []

cumulativeSum :: Array Int Int -> [Int]
cumulativeSum xs = scanl (+) 0 $ elems xs

main :: IO ()
main = do
    d <- readLn
    n <- readLn
    lrs <- concatMap (add2PreviousDay . fmap read . words) <$> replicateM n getLine
    let accArray = accumArray (+) 0 (1, d + 1) lrs
    mapM_ print $ tail $ init $ cumulativeSum accArray
