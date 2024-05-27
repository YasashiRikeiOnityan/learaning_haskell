module ABC355D where
    
import Control.Monad (replicateM)
import Data.List (sort, foldl')

main :: IO ()
main = do
    n <- readLn :: IO Int
    lrs <- map ((\[l, r] -> (l, r)) . map read . words) <$> replicateM n getLine :: IO [(Int, Int)]
    let xs = [[(l, 1 :: Int), (r + 1, -1)] | (l, r) <- lrs]
        xs_ = sort (concat xs)
    print . fst $ count xs_


count :: [(Int, Int)] -> (Int, Int)
count = foldl' f (0, 0)
    where
        f (l, r) (_, x)
            | x == 1 = (l + r, r + 1)
            | otherwise = (l, r - 1)