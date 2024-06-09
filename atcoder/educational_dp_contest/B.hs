module B where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!), bounds )
import qualified Data.IntMap as IM

dynamicProgramming :: Int -> Array Int Int -> IM.IntMap Int -> (Int, IM.IntMap Int)
dynamicProgramming n hrr im
    | n == 1 = (0, im)
    | n == 2 = (abs (hrr ! n - hrr ! (n - 1)), im)
    | otherwise = case IM.lookup n im of
        Just x  -> (x, im)
        Nothing -> (cost, im3)
        where
            (cost1, im1) = dynamicProgramming (n - 1) hrr im
            (cost2, im2) = dynamicProgramming (n - 2) hrr im1
            cost = min (cost1 + abs (hrr ! n - hrr ! (n - 1))) (cost2 + abs (hrr ! n - hrr ! (n - 2)))
            im3 = IM.insert n cost im2

main :: IO ()
main = do
    [n, k] <- map read . words <$> getLine :: IO [Int]
    hrr <- listArray (1, n) . map readInt . BS.words <$> BS.getLine
    print . fst $ dynamicProgramming n hrr IM.empty

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt