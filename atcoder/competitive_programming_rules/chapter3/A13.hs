module A13 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )

calc :: Int -> Int -> (Int, Int) -> Array Int Int -> Int -> (Int, Int)
calc l r (start, sum_) arr k
    | start == r = (start, sum_)
    | otherwise = if (arr ! (start + 1) - arr ! l) <= k then calc l r (start + 1, sum_) arr k else (start, sum_ + start - l)

syakutori :: Int -> Int -> Int -> Array Int Int -> Int -> Int -> Int 
syakutori l r start arr k sum_
    | l == r = sum_
    | otherwise = syakutori (l + 1) r start_ arr k sum__
        where
            (start_, sum__) = calc l r (start, sum_) arr k

main :: IO ()
main = do
    [n, k] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    let arr = listArray (1, n) as
    print $ syakutori 1 (n-1) 1 arr k 0

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt