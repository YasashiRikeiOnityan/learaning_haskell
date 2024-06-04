module ABC356C where

import Control.Monad ( replicateM )
import Data.Array ( listArray, (!) )

main :: IO ()
main = do
    [n, m, k] <- map read . words <$> getLine :: IO [Int]
    casrss <- map (map (\x -> (case x of
        "o" -> 0
        "x" -> -1
        _ -> read x)) . words) <$> replicateM m getLine :: IO [[Int]]
    let keyPatterns = map (zip [1..n]) (replicateM n [True, False])
    print . length . filter id $ map (\keyPattern -> all (\casrs -> test n k casrs keyPattern) casrss) keyPatterns

test :: Int ->  Int -> [Int] -> [(Int, Bool)] -> Bool
test n k casrs keyPattern = 
    let as = init (tail casrs)
        r = last (tail casrs)
        keyArr = listArray (1, n) keyPattern
        count = foldl (\acc x -> if snd (keyArr ! x) then succ acc else acc) 0 as
    in if r == 0 then count >= k 
                 else count < k