module A18 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!), bounds, elems, assocs )

mkMap :: (Int, Int) -> Array Int Int -> (Int, Int) -> Array Int Bool -> Array Int Bool
mkMap (n, s) arr (x, y) brr
    | x == 0 = mkMap (n, s) arr (0, 1) (listArray (0, s) (True : replicate s False))
    | x == n = mkNewArray (n, s) arr (x, y) brr
    | otherwise = mkMap (n, s) arr (0, succ y) (mkNewArray (n, s) arr (x, y) brr)

mkNewArray :: (Int, Int) -> Array Int Int -> (Int, Int) -> Array Int Bool -> Array Int Bool
mkNewArray (n, s) arr (x, y) brr
    = listArray (0, s) $ zipWith (||) (elems brr) (f 0 s  (map (+ (arr ! y)) (trueIndices brr)) [])

-- Trueのインデックスをリストに抽出する関数
trueIndices :: Array Int Bool -> [Int]
trueIndices arr = [i | (i, True) <- assocs arr]

f :: Int -> Int -> [Int] -> [Bool] -> [Bool]
f i s ids bs
    | i == s = bs ++ if i `elem` ids then [True] else [False]
    | otherwise = f (succ i) s ids (bs ++ if i `elem` ids then [True] else [False]) 

main :: IO ()
main = do
    [n, s] <- map readInt . BS.words <$> BS.getLine
    arr <- listArray (1, n) . map readInt . BS.words <$> BS.getLine
    print arr

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt