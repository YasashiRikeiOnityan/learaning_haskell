module B13 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )

step :: Int -> Int -> Int -> Array Int Int -> Int -> Int
step l r start arr k
    | start < r && arr ! (start + 1) - arr ! (l - 1) <= k = step l r (start + 1) arr k
    | otherwise = start

syakutori :: Int -> Int -> Int -> Array Int Int -> Int ->Int
syakutori l r start arr k 
    | l == r = next - l + 1
    | otherwise = (next - l + 1) + syakutori (l + 1) r next arr k
        where
            next = step l r start arr k

main :: IO ()
main = do
    [n, k] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    let arr = listArray (0, n) $ scanl (+) 0 as
    print $ syakutori 1 n 0 arr k

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt