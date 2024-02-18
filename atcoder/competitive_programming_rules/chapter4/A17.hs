module A17 where

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
            | i == 1 = (0, IM.insert i 0 im_)
            | i == 2 = (arr_ ! i, IM.insert i (arr_ ! i) im_)
            | otherwise = case IM.lookup i im_ of
                Just x  -> (x, im_)
                Nothing -> (cost, im3)
                where
                    (cost1, im1) = dp (i - 1) arr_ brr_ im_
                    (cost2, im2) = dp (i - 2) arr_ brr_ im1
                    cost = min (cost1 + arr_ ! i) (cost2 + brr_ ! i)
                    im3 = IM.insert i cost im2

reconstruction :: Int -> Array Int Int -> IM.IntMap Int -> (Int, [Int]) -> (Int, [Int])
reconstruction n arr im (len, list)
    | n == 1 = (len_, 1 : list)
    | otherwise = reconstruction n_ arr im (len_, n : list)
        where
            len_ = len + 1
            x = (+) <$> IM.lookup (n - 1) im <*> Just (arr ! n)
            y = IM.lookup n im
            n_ = if x == y then n - 1 else n - 2

main :: IO ()
main = do
    n <- readLn
    arr <- listArray (2, n) . map readInt . BS.words <$> BS.getLine
    brr <- listArray (3, n) . map readInt . BS.words <$> BS.getLine
    let ans = reconstruction n arr (snd (dynamicProgramming arr brr IM.empty)) (0, [])
    print $ fst ans
    putStrLn . unwords . map show $ snd ans

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt