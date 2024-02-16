module B16 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!), bounds )
import qualified Data.IntMap as IM

dynamicProgramming :: Array Int Int -> IM.IntMap Int -> (Int, IM.IntMap Int)
dynamicProgramming hrr im = dp n hrr im
    where
        n = snd (bounds hrr)
        dp i hrr_ im_
            | i == 1 = (0, im_)
            | i == 2 = (abs (hrr_ ! i - hrr_ ! (i - 1)), im_)
            | otherwise = case IM.lookup i im_ of
                Just x  -> (x, im_)
                Nothing -> (cost, im3)
                where
                    (cost1, im1) = dp (i - 1) hrr_ im_
                    (cost2, im2) = dp (i - 2) hrr_ im1
                    cost = min (cost1 + abs (hrr_ ! i - hrr_ ! (i - 1))) (cost2 + abs (hrr_ ! i - hrr_ ! (i - 2)))
                    im3 = IM.insert i cost im2

main :: IO ()
main = do
    n <- readLn
    hrr <- listArray (1, n) . map readInt . BS.words <$> BS.getLine
    print . fst $ dynamicProgramming hrr IM.empty

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt