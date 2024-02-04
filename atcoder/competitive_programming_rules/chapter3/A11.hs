module A11 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )

binarySearch :: Int -> Int -> Int -> Array Int Int -> Int
binarySearch x min max as = if x == num then mid else binarySearch x newMin newMax as
    where mid = (max + min) `div` 2
          num = as ! mid
          newMin = if x < num then min else mid + 1
          newMax = if x < num then mid - 1 else max

main :: IO ()
main = do
    [n, x] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    let arr = listArray (1, n) as
    print $ binarySearch x 1 n arr

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

showInt :: Int -> ByteString
showInt = BS.pack . show