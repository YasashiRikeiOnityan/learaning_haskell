module A12 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )

check :: Array Integer Integer -> Integer -> Integer -> Integer-> Bool
check arr n k mid = sum_ arr mid n >= k
    where
        sum_ :: Array Integer Integer -> Integer -> Integer -> Integer
        sum_ arr_ mid_ 1 = mid_ `div` (arr_ ! 1)
        sum_ arr_ mid_ i = mid_ `div` (arr_ ! i) + sum_ arr_ mid_ (i - 1)

binarySearch :: Integer -> Integer -> Array Integer Integer -> Integer -> Integer -> Integer
binarySearch l r arr n k
    | l == r = l
    | check arr n k mid = binarySearch l mid arr n k
    | otherwise = binarySearch (mid + 1) r arr n k
        where
            mid = (l + r) `div` 2

main :: IO ()
main = do
    [n, k] <- map readInteger . BS.words <$> BS.getLine
    as <- map readInteger . BS.words <$> BS.getLine
    let arr = listArray (1, n) as
    let left = 1
    let right = 1000000000
    print $ binarySearch left right arr n k
    
readInteger :: ByteString -> Integer
readInteger = fst . fromJust . BS.readInteger
