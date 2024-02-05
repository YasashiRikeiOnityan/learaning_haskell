module B11 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )
import Data.List ( sort )
import Control.Monad ( replicateM )
{-
binarySearch :: Int -> Int -> Int -> Array Int Int -> Int
binarySearch x min max as
    | mid == 1 = if x <= numL then mid - 1 else mid
    | mid == max = if numR < x then mid else mid - 1
    | otherwise = if numL < x && x <= numR then mid else binarySearch x newMin newMax as
        where mid = (max + min) `div` 2
              numL = as ! mid
              numR = as ! (mid + 1)
              newMin = if x < numL then min else mid + 1
              newMax = if x < numL then mid - 1 else max
-}

lowerBound :: Ord a => a -> [a] -> Maybe a
lowerBound x xs = case binarySearch x xs of
                    Just index -> Just (xs !! index)
                    Nothing    -> Nothing

binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch _ [] = Nothing
binarySearch x xs = search 0 (length xs - 1)
  where
    search low high
      | low > high = Nothing
      | otherwise =
        let mid = (low + high) `div` 2
            midValue = xs !! mid
        in if x <= midValue
             then case search low (mid - 1) of
                    Just result -> Just result
                    Nothing     -> Just mid
             else search (mid + 1) high

main :: IO ()
main = do
    [n] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    [q] <- map readInt . BS.words <$> BS.getLine
    xs <- concatMap (map readInt . BS.words) <$> replicateM q BS.getLine
    -- let arr = listArray (1, n) $ sort as
    mapM_ (print . fromJust . \x -> binarySearch x as) xs

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

showInt :: Int -> ByteString
showInt = BS.pack . show