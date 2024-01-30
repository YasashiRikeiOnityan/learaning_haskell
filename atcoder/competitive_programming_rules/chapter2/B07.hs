import Control.Monad ( replicateM )
import Data.Array ( accumArray, elems, Array )

attendAndLeave :: [Int] -> [(Int, Int)]
attendAndLeave [l, r] = [(l, 1), (r, -1)]
attendAndLeave _ = []

cumulativeSum :: Array Int Int -> [Int]
cumulativeSum xs = scanl (+) 0 $ elems xs

main :: IO ()
main = do
    t <- readLn :: IO Int
    n <- readLn :: IO Int
    lrs <- concatMap (attendAndLeave . fmap read . words) <$> replicateM n getLine :: IO [(Int, Int)]
    let accArray = accumArray (+) 0 (0, t) lrs
    mapM_ print $ tail $ init $ cumulativeSum accArray
