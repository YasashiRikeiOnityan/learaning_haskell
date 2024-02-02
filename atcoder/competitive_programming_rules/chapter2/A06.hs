module A06 where
    
import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray )

cumulativeSum :: Int -> Int -> [Int] -> Array Int Int
cumulativeSum l r as = listArray (l, r) $ scanl (+) 0 as

solve :: Array Int Int -> (Int, Int) -> Int
solve s (l, r) = (s ! r) - (s ! (l - 1))

main :: IO ()
main = do
    [n, q] <- map read . words <$> getLine
    as <- map read . words <$> getLine
    lrs <- map ((\[l, r] -> (l, r)) . map read . words) <$> replicateM q getLine
    let cumSum = cumulativeSum 0 n as
    mapM_ (print . solve cumSum) lrs
