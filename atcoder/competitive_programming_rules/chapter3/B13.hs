module B13 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )

step :: Int -> Int -> (Int, Int) -> Int -> Array Int Int -> Int -> (Int, Int)
step l r (start, sum_) price arr k
    | start < r && price_ <= k = step l r (start + 1, sum_) price_ arr k
    | start == r && price_ <= k = (start, sum_ + start - l + 1)
    | otherwise = (start, sum_ + start - l)
    where
        price_ = price + arr ! start

syakutori :: Int -> Int -> Int -> Array Int Int -> Int -> Int -> Int 
syakutori l r start arr k sum_
    | l == r = if arr ! r <= k then sum_ + 1 else sum_
    | otherwise = syakutori (l + 1) r newStart arr k newSum
        where
            (newStart, newSum) = step l r (start, sum_) 0 arr k

main :: IO ()
main = do
    [n, k] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    let left = 1
        right = n
        arr = listArray (left, right) as
        sum_ = 0
    print $ syakutori left right left arr k sum_

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt