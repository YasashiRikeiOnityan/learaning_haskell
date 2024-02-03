module B08 where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray, accumArray, elems, bounds )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)


-- build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
-- build g = g (:) []
-- 
-- chunksOf :: Int -> [e] -> [[e]]
-- chunksOf i ls = map (take i) (build (splitter ls))
--     where splitter :: [e] -> ([e] -> a -> a) -> a -> a
--           splitter [] _ n = n
--           splitter l c n = l `c` splitter (drop i l) c n

twoDimensionalSum :: Array (Int, Int) Int -> Array (Int, Int) Int
twoDimensionalSum arr = listArray bounds_
                         $ concat
                         $ scanl1 (zipWith (+))
                         $ map (scanl1 (+)) lists
    where bounds_ = bounds arr
          lists = chunksOf (1 + (snd . snd) bounds_) $ elems arr

arrayToLists :: Array (Int, Int) Int -> [[Int]]
arrayToLists arr = [[arr ! (i, j) | j <- [0..jMax]] | i <- [0..iMax]]
    where rightBound = snd (bounds arr)
          iMax = fst rightBound
          jMax = snd rightBound

solve :: Array (Int, Int) Int -> [(Int, Int, Int, Int)] -> [Int]
solve twoDimSum = map (\(a, b, c, d) -> twoDimSum ! (a - 1, b - 1) + twoDimSum ! (c, d) 
                                      - twoDimSum ! (c, b - 1) - twoDimSum ! (a - 1, d))

main :: IO ()
main = do
    n <- readLn
    xys <- map ((\[x, y] -> ((x, y), 1)) . map readInt . BS.words) <$> replicateM n BS.getLine
    q <- readLn
    qs <- map ((\[a, b, c, d] -> (a, b, c, d)) . map readInt . BS.words) <$> replicateM q BS.getLine
    let w = maximum $ map (\((x, _), _) -> x) xys
    let h = maximum $ map (\((_, y), _) -> y) xys
    let twoDimSUm = twoDimensionalSum $ accumArray (+) 0 ((0, 0), (w, h)) xys
    mapM_ (BS.putStrLn . showInt) (solve twoDimSUm qs)

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

showInt :: Int -> ByteString
showInt = BS.pack . show