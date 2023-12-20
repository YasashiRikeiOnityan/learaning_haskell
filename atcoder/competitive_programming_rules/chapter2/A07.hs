import Control.Monad
import Data.Array

cumulativeSum :: (Int, Int) -> Array Int Int -> Array Int Int
cumulativeSum lr xs = listArray lr $ scanl (+) 0 $ elems xs

solve :: (Int, Int) -> Array Int Int -> Int
solve (l, r) as = as ! r - as ! (l - 1)

main :: IO ()
main = do
    d <- readLn :: IO Int
    n <- readLn :: IO Int
    lrs <- map (map read . words) <$> replicateM n getLine :: IO [[Int]]
    print lrs