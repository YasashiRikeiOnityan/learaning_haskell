module A18 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!), bounds )

mkMap :: (Int, Int) -> Array Int Int -> (Int, Int) -> [Bool] -> [Bool]
mkMap (n, s) arr (x, y) list
    | x == 0 = mkMap (n, s) arr (0, 1) (True : replicate s False)
    | otherwise = mkMap (n, s) arr (0, pred y) (mkNewList arr y list)
        where
            mkNewList arr y list = []

main :: IO ()
main = do
    [n, s] <- map readInt . BS.words <$> BS.getLine
    arr <- listArray (1, n) . map readInt . BS.words <$> BS.getLine
    print arr

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt