module B09 where

import Control.Monad ( replicateM )
import Data.Array ( Array, listArray, accumArray, elems, bounds )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

showInt :: Int -> ByteString
showInt = BS.pack . show

getPairs :: [Int] -> [((Int, Int), Int)]
getPairs [x,y,z,w] = [((x, y), 1), ((z, w), 1), ((x, w), -1), ((z, y), -1)]
getPairs _ = []

twoDimensionalSum :: Array (Int, Int) Int -> Array (Int, Int) Int
twoDimensionalSum arr = listArray bounds_
                         $ concat
                         $ scanl1 (zipWith (+))
                         $ map (scanl1 (+)) lists
    where bounds_ = bounds arr
          lists = chunksOf (1 + (snd . snd) bounds_) $ elems arr

area :: Array (Int, Int) Int -> Int
area arr = length . concatMap (filter (/= 0)) $ lists
    where bounds_ = bounds arr
          lists = chunksOf ((snd . snd) bounds_) $ elems arr

main :: IO ()
main = do
    n <- readLn
    xys <- concatMap (getPairs . map readInt . BS.words) <$> replicateM n BS.getLine
    let h = maximum $ map (\((x, _), _) -> x) xys
    let w = maximum $ map (\((_, y), _) -> y) xys
    let arr = accumArray (+) 0 ((0,0), (h, w)) xys
    print . area . twoDimensionalSum $ arr
    