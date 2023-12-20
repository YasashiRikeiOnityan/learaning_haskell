import Control.Monad ( replicateM_ )
import Data.Array ( Array, (!), elems, listArray )

cumulativeSum :: (Int, Int) -> Array Int Int -> Array Int Int
cumulativeSum lr xs = listArray lr $ scanl (+) 0 $ elems xs

solve :: Array Int Int -> (Int, Int) -> Int
solve s (l, r) = (s ! r) - (s ! (l - 1))

main :: IO ()
main = do
    n <- readLn :: IO Int
    as <- listArray (0, n - 1) . map read . words <$> getLine :: IO (Array Int Int)
    q <- readLn :: IO Int
    let cumSum = cumulativeSum (0, n) as
    replicateM_ q $ do 
        [l, r] <- map read . words <$> getLine :: IO [Int]
        let t = solve cumSum (l, r)
        putStrLn $ case (r - l + 1 - t) `compare` t of
            GT -> "lose"
            EQ -> "draw"
            LT -> "win"
