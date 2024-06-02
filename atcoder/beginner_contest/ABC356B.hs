module ABC356B where
    
import Control.Monad ( replicateM )

main :: IO ()
main = do
    [n, m] <- map read . words <$> getLine :: IO [Int]
    as <- map read . words <$> getLine :: IO [Int]
    xss <- map (map read . words) <$> replicateM n getLine :: IO [[Int]]
    putStrLn $ if judge as (foldl1 (zipWith (+)) xss) then "Yes" else "No"

judge :: [Int] -> [Int] -> Bool
judge [] _ = True
judge (a : as) (x : xs) = a <= x && judge as xs