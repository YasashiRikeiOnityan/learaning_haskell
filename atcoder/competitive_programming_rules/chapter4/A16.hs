module A16 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!), bounds )
import qualified Data.IntMap as IM

dynamicProgramming :: Array Int Int -> Array Int Int -> IM.IntMap Int -> (Int, IM.IntMap Int)
dynamicProgramming arr brr im = dp n arr brr im
    where
        n = snd (bounds arr)
        dp i arr_ brr_ im_
            | i == 1 = (0, im_)
            | i == 2 = (arr_ ! i, im_)
            | otherwise = case IM.lookup i im_ of
                Just x  -> (x, im_)
                Nothing -> (cost, im3)
                where
                    (cost1, im1) = dp (i - 1) arr_ brr_ im_
                    (cost2, im2) = dp (i - 2) arr_ brr_ im1
                    cost = min (cost1 + arr_ ! i) (cost2 + brr_ ! i)
                    im3 = IM.insert i cost im2

main :: IO ()
main = do
    n <- readLn
    arr <- listArray (2, n) . map readInt . BS.words <$> BS.getLine
    brr <- listArray (3, n) . map readInt . BS.words <$> BS.getLine
    print . fst $ dynamicProgramming arr brr IM.empty

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt