module A15 where

import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )

erase :: [Int] -> [Int]
erase xs = Set.toList $ Set.fromList xs

binarySearch :: Int -> Int -> Array Int Int -> Int -> Int
binarySearch l r arr x
    | x == arr ! mid = mid
    | x < arr ! mid = binarySearch l (mid - 1) arr x
    | otherwise = binarySearch (mid + 1) r arr x
        where
            mid = (l + r) `div` 2

main :: IO ()
main = do
    _ <- getLine
    as <- map readInt . BS.words <$> BS.getLine
    let as_ = erase as
        len = length as_
        arr = listArray (1, len) as_
    putStrLn . unwords $ map (show . binarySearch 1 len arr) as

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt
