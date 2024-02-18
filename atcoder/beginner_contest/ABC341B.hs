module ABC341B where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Control.Monad ( replicateM )
import Data.Array ( Array, listArray, (!), bounds )
import qualified Data.IntMap as IM

toPair :: [Int] -> [(Int, Int)]
toPair [x, y] = [(x, y)]
toPair _ = []

solve :: Int -> Int -> IM.IntMap Int -> Array Int (Int, Int) -> IM.IntMap Int
solve i n arr strr
    | i == n = arr
    | otherwise = solve (i + 1) n newArr strr
    where
        money = fromJust (IM.lookup i arr)
        cost = fst (strr ! i)
        now = fromJust (IM.lookup (i + 1) arr)
        newArr = IM.insert (i + 1) (now + (money `div` cost) * snd (strr ! i)) arr

main :: IO ()
main = do
    n <- readLn :: IO Int
    arr <- IM.fromList . zip [1..] . map readInt . BS.words <$> BS.getLine
    strr <- listArray (1, n - 1) . concatMap (toPair . map readInt . BS.words) <$> replicateM (n - 1) BS.getLine
    print . snd . IM.findMax $ solve 1 n arr strr

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt