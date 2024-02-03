module A09 where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray, accumArray, elems, bounds )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

showInt :: Int -> ByteString
showInt = BS.pack . show

getPairs :: [Int] -> [((Int, Int), Int)]
getPairs [x,y,z,w] = [((x, y), 1), ((z + 1, w + 1), 1), ((x, w + 1), -1), ((z + 1, y), -1)]
getPairs _ = []

twoDimensionalSum :: Array (Int, Int) Int -> Array (Int, Int) Int
twoDimensionalSum arr = listArray bounds_
                         $ concat
                         $ scanl1 (zipWith (+))
                         $ map (scanl1 (+)) lists
    where bounds_ = bounds arr
          lists = chunksOf ((snd . snd) bounds_) $ elems arr

printArray :: Array (Int, Int) Int -> IO ()
printArray arr = do
    let ((minX, minY), (maxX, maxY)) = bounds arr
    mapM_ (\i -> BS.putStrLn $ BS.unwords [showInt (arr ! (i,j)) | j <- [minY .. maxY - 1]]) [minX .. maxX - 1]

main :: IO ()
main = do
    [h, w, n] <- map readInt . BS.words <$> BS.getLine
    xys <- concatMap (getPairs . map readInt . BS.words) <$> replicateM n BS.getLine
    printArray $ twoDimensionalSum $ accumArray (+) 0 ((1,1), (h + 1, w + 1)) xys
    