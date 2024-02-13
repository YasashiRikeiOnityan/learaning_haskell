module B13 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )

step :: Int -> Int -> (Int, Int, Int) -> Array Int Int -> Int -> (Int, Int, Int)
step l r (start, price, sum_) arr k
    | start <= r && price_ <= k = step l r (start + 1, price_, sum_) arr k
    | otherwise = (start, price__, sum_ + start - l)
    where
        price_ = price + arr ! start
        price__ = price_ - (arr ! l) - (arr ! start)

syakutori :: Int -> Int -> Int -> Array Int Int -> Int -> Int -> Int -> Int 
syakutori l r start arr k price sum_
    | l > r = sum_
    | otherwise = syakutori (l + 1) r newStart arr k newPrice newSum
        where
            (newStart, newPrice, newSum) = step l r (start, price, sum_) arr k

main :: IO ()
main = do
    [n, k] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    let left = 1
        right = n
        start = left
        arr = listArray (left, right) as
        price = 0
        sum_ = 0
    print $ syakutori left right start arr k price sum_

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt