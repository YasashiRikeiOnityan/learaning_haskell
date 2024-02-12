module A13 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )

step :: Int -> Int -> (Int, Int) -> Array Int Int -> Int -> (Int, Int)
step l r (start, sum_) arr k
    | start < r && (arr ! (start + 1) - arr ! l) <= k = step l r (start + 1, sum_) arr k
    | otherwise = (start, sum_ + start - l)

syakutori :: Int -> Int -> Int -> Array Int Int -> Int -> Int -> Int 
syakutori l r start arr k sum_
    | l > r = sum_
    | otherwise = syakutori (l + 1) r newStart arr k newSum
        where
            (newStart, newSum) = step l r (start, sum_) arr k

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